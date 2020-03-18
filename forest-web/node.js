"use strict";

/*
 * Utility functions
 */

// Create a new DOM element.
// 'classes' can either be a string or a list of strings.
// A child can either be a string or a DOM element.
function newElement(type, classes, ...children) {
    let e = document.createElement(type);

    if (classes !== undefined) {
        if (typeof classes == "string") {
            e.classList.add(classes);
        } else if (classes instanceof Array) {
            e.classList.add(...classes);
        }
    }

    children.forEach(child => {
        if (typeof child == "string") {
            e.appendChild(document.createTextNode(child));
        } else {
            e.appendChild(child);
        }
    });

    return e;
}

/*
 * Classes
 */

// Enum representing useful positions relative to a node.
const RelPos = Object.freeze({
    FIRST_CHILD: 1,
    NEXT_SIBLING: 2,
});

class Path {
    constructor(...nodeIds) {
        this.elements = nodeIds;
    }

    get length() {
        return this.elements.length;
    }

    get last() {
        return this.elements[this.length - 1];
    }

    get parent() {
        if (this.length === 0) return undefined;
        return new Path(...this.elements.slice(0, this.length - 1));
    }

    append(nodeId) {
        return new Path(...this.elements.concat([nodeId]));
    }

    concat(otherPath) {
        return new Path(...this.elements.concat(otherPath.elements));
    }
}

class NodeElements {
    constructor() {
        this.text = newElement("span", "node-text");
        this.permissions = newElement("span", "node-permissions");
        this.children = newElement("div", "node-children");

        let line = newElement("div", "node-line", this.text, this.permissions);
        this.main = newElement("div", ["node", "is-folded"], line, this.children);
    }

    removeAllChildren() {
        while (this.children.firstChild) {
            this.children.removeChild(this.children.lastChild);
        }
    }
}

class Node {
    constructor(nodeJson) {
        this.elements = undefined;

        this.text = nodeJson.text;

        // Permissions
        this.edit = nodeJson.edit;
        this.delete = nodeJson.delete;
        this.reply = nodeJson.reply;
        this.act = nodeJson.act;

        this.children = new Map();
        this.order = nodeJson.order;
        this.order.forEach(childId => {
            let childJson = nodeJson.children[childId];
            let childNode = new Node(childJson);
            this.children.set(childId, childNode);
        });
    }

    getPermissionText() {
        return [
            "(",
            this.edit   ? "e" : "-",
            this.delete ? "d" : "-",
            this.reply  ? "r" : "-",
            this.act    ? "a" : "-",
            ")"
        ].join("");
    }

    hasChildren() {
        return this.order.length > 0;
    }

    isFolded() {
        if (this.elements === undefined) return undefined;
        return this.elements.main.classList.contains("is-folded");
    }

    setFolded(folded) {
        if (this.elements === undefined) return;
        this.elements.main.classList.toggle("is-folded", folded);
    }

    toggleFolded() {
        this.setFolded(!this.isFolded());
    }

    // Obtain and update this node's DOM elements. After this call, this.el
    // represents the current node's contents.
    //
    // This function may optionally be called with an old node. If that node or
    // its children already has existing DOM elements, they are repurposed.
    // Otherwise, new DOM elements are created.
    obtainElements(oldNode) {
        if (this.elements === undefined) {
            // Obtain DOM elements because we don't yet have any
            if (oldNode === undefined || oldNode.elements === undefined) {
                this.elements = new NodeElements();
            } else {
                this.elements = oldNode.elements;
            }
        }

        this.elements.text.textContent = this.text;
        this.elements.permissions.textContent = this.getPermissionText();
        this.elements.main.classList.toggle("has-children", this.hasChildren());

        let oldChildren = (oldNode === undefined) ?
            new Map() : oldNode.children;

        this.elements.removeAllChildren();
        this.order.forEach(childId => {
            let oldChild = oldChildren.get(childId); // May be undefined
            let child = this.children.get(childId);
            child.obtainElements(oldChild);
            this.elements.children.appendChild(child.elements.main);
        });
    }
}

class NodeTree {
    constructor(rootNodeContainer, rootNode) {
        this.rootNodeContainer = rootNodeContainer;
        this.rootNode = rootNode;

        // Prepare root node container
        rootNode.obtainElements();
        while (rootNodeContainer.firstChild) {
            rootNodeContainer.removeChild(rootNodeContainer.lastChild);
        }
        rootNodeContainer.appendChild(rootNode.elements.main);
    }

    at(path) {
        let node = this.rootNode;
        for (let childId of path.elements) {
            node = node.children.get(childId);
            if (node === undefined) break;
        }
        return node;
    }

    updateAt(path, newNode) {
        if (path.length === 0) {
            newNode.obtainElements(this.rootNode);
            this.rootNode = newNode;
        } else {
            let parentNode = this.at(path.parent);
            let oldNode = parentNode.children.get(path.last);
            if (oldNode === undefined) return;
            newNode.obtainElements(oldNode);
            parentNode.children.set(path.last, newNode);
        }
    }

    getChildWith(path, f) {
        let node = this.at(path);
        if (node === undefined) return undefined;
        let index = f(node.order.length);
        if (index === undefined) return undefined;
        let childId = node.order[index];
        if (childId === undefined) return undefined;
        return path.append(childId);
    }

    getFirstChild(path) {
        return this.getChildWith(path, l => 0);
    }

    getLastChild(path) {
        return this.getChildWith(path, l => l - 1);
    }

    getSiblingWith(path, f) {
        if (path.parent === undefined) return undefined;
        let parentNode = this.at(path.parent);
        if (parentNode === undefined) return undefined;

        let index = parentNode.order.indexOf(path.last);
        if (index === undefined) return undefined;
        let newIndex = f(index);
        if (newIndex === undefined) return undefined;
        let siblingId = parentNode.order[newIndex];
        if (siblingId === undefined) return undefined;

        return path.parent.append(siblingId);
    }

    getPrevSibling(path) {
        return this.getSiblingWith(path, i => i - 1);
    }

    getNextSibling(path) {
        return this.getSiblingWith(path, i => i + 1);
    }

    getNodeAbove(path) {
        let prevPath = this.getPrevSibling(path);
        if (prevPath === undefined) return path.parent;

        // Get last child of previous path
        while (true) {
            let prevNode = this.at(prevPath);
            if (prevNode.isFolded()) return prevPath;

            let childPath = this.getLastChild(prevPath);
            if (childPath === undefined) return prevPath;

            prevPath = childPath;
        }
    }

    getNodeBelow(path) {
        let node = this.at(path);
        if (!node.isFolded()) {
            let childPath = this.getFirstChild(path);
            if (childPath !== undefined) return childPath;
        }

        while (path !== undefined) {
            let nextPath = this.getNextSibling(path);
            if (nextPath !== undefined) return nextPath;
            path = path.parent;
        }

        return undefined;
    }
}

class Cursor {
    constructor(nodeTree) {
        this.nodeTree = nodeTree;

        this.path = new Path();
        this.relPos = null; // Either null or a RelPos value

        this.restore();
    }

    getSelectedNode() {
        return this.nodeTree.at(this.path);
    }

    _applyRelPos() {
        if (this.relPos === null) return;

        let newPath;
        if (this.relPos === RelPos.FIRST_CHILD) {
            newPath = this.nodeTree.getFirstChild(this.path);
        } else if (this.relPos === RelPos.NEXT_SIBLING) {
            newPath = this.nodeTree.getNextSibling(this.path);
        }

        if (newPath !== undefined) {
            this.path = newPath;
            this.relPos = null;
        }
    }

    _moveToNearestValidNode() {
        // TODO Maybe select a sibling instead of going to nearest visible parent
        let path = new Path();
        for (let element of this.path.elements) {
            let newPath = path.append(element);
            let newNode = this.nodeTree.at(newPath);
            if (newNode === undefined) break;
            if (newNode.isFolded()) break;
            path = newPath;
        }
        this.path = path;
    }

    _set(visible) {
        this.getSelectedNode().elements.main.classList.toggle("has-cursor", visible);
    }

    restore() {
        this._applyRelPos();
        this._moveToNearestValidNode();
        this._set(true);
    }

    moveTo(path) {
        if (path === undefined) return;
        this._set(false);
        this.path = path;
        this._set(true);
    }

    moveUp() {
        this.moveTo(this.nodeTree.getNodeAbove(this.path));
    }

    moveDown() {
        this.moveTo(this.nodeTree.getNodeBelow(this.path));
    }
}

class Editor {
    constructor(nodeTree) {
        this.nodeTree = nodeTree;

        this.textarea = newElement("textarea");
        this.element = newElement("div", "node-editor", this.textarea);
        this.textarea.addEventListener("input", event => this._updateTextAreaHeight());

        this.path = undefined;
        this.asChild = false;
    }

    _updateTextAreaHeight() {
        this.textarea.style.height = 0;
        this.textarea.style.height = this.textarea.scrollHeight + "px";
    }

    _getAttachedNode() {
        if (this.path === undefined) return undefined;
        return this.nodeTree.at(this.path);
    }

    _detach(node, asChild) {
        if (!asChild) {
            node.elements.main.classList.remove("has-editor");
        }

        this.element.parentNode.removeChild(this.element);
    }

    _attachTo(node, asChild) {
        if (asChild) {
            node.elements.children.appendChild(this.element);
            node.setFolded(false);
        } else {
            node.elements.main.classList.add("has-editor");
            node.elements.main.insertBefore(this.element, node.elements.children);
        }
        this._updateTextAreaHeight();
    }

    restore() {
        if (this.element.parentNode !== null) return; // Already attached
        let node = this._getAttachedNode();
        if (node === undefined) return; // Nowhere to attach
        this._attachTo(node, this.asChild);
    }

    attachTo(path, asChild) {
        this.detach();
        this.path = path;
        this.asChild = asChild;
        this.restore();

        this.textarea.focus();
        let length = this.textarea.value.length;
        this.textarea.setSelectionRange(length, length);
    }

    detach() {
        let node = this._getAttachedNode();
        if (node === undefined) return;
        this._detach(node, this.asChild);
        this.path = undefined;
    }

    set content(text) {
        this.textarea.value = text;
    }

    get content() {
        return this.textarea.value;
    }
}

class Connection {
    constructor(nodeTree, cursor, editor, url) {
        this.nodeTree = nodeTree;
        this.cursor = cursor;
        this.editor = editor;

        this.url = url;
        this.ws = new WebSocket(this.url);
        this.ws.addEventListener("message", msg => this.onMessage(msg));
        this.ws.addEventListener("open", _ => this.sendHello());
    }

    onMessage(msg) {
        let content = JSON.parse(msg.data);
        if (content.type === "hello") {
            this.onHello(content);
        } else if (content.type === "update") {
            this.onUpdate(content);
        }
    }

    onHello(content) {
        this.nodeTree.updateAt(new Path(), new Node(content.node));
        this.cursor.restore();
        this.editor.restore();
    }

    onUpdate(content) {
        this.nodeTree.updateAt(new Path(...content.path), new Node(content.node));
        this.cursor.restore();
        this.editor.restore();
    }

    _send(thing) {
        this.ws.send(JSON.stringify(thing));
    }

    sendHello() {
        this._send({type: "hello", extensions: []});
    }

    sendEdit(path, text) {
        this._send({type: "edit", path: path.elements, text: text});
    }

    sendDelete(path) {
        this._send({type: "delete", path: path.elements});
    }

    sendReply(path, text) {
        this._send({type: "reply", path: path.elements, text: text});
    }

    sendAct(path) {
        this._send({type: "act", path: path.elements});
    }
}

/*
 * The main application
 */

const rootNodeContainer = document.getElementById("root-node-container");
const loadingNode = new Node({text: "Connecting...", children: {}, order: []});
const nodeTree = new NodeTree(rootNodeContainer, loadingNode);
const cursor = new Cursor(nodeTree);
const editor = new Editor(nodeTree);
const conn = new Connection(nodeTree, cursor, editor, "ws://127.0.0.1:8080/");

function beginEdit() {
    let node = cursor.getSelectedNode();
    editor.content = node.text;
    editor.attachTo(cursor.path, false);
}

function beginDirectReply() {
    editor.content = "";
    editor.attachTo(cursor.path, true);
}

function beginIndirectReply() {
    let path = cursor.path.parent;
    if (path === undefined) return;
    editor.content = "";
    editor.attachTo(path, true);
}

function cancelEdit() {
    editor.detach();
}

function completeEdit() {
    let path = editor.path;
    let text = editor.textarea.value;
    if (editor.asChild) {
        conn.sendReply(path, text);
    } else {
        conn.sendEdit(path, text);
    }
    editor.detach();
}

document.addEventListener("keydown", event => {
    if (event.code === "Escape") {
        cancelEdit();
        event.preventDefault();
    } else if (event.code === "Enter" && !event.shiftKey) {
        completeEdit();
        event.preventDefault();
    } else if (document.activeElement.tagName === "TEXTAREA") {
        return; // Do nothing special
    } else if (event.code === "Tab") {
        cursor.getSelectedNode().toggleFolded();
        event.preventDefault();
    } else if (event.code === "KeyK" || event.code === "ArrowUp") {
        cursor.moveUp();
        event.preventDefault();
    } else if (event.code === "KeyJ" || event.code === "ArrowDown") {
        cursor.moveDown();
        event.preventDefault();
    } else if (event.code === "KeyE") {
        beginEdit();
        event.preventDefault();
    } else if (event.code === "KeyR") {
        if (event.shiftKey) {
            console.log("indirect");
            beginIndirectReply();
        } else {
            console.log("direct");
            beginDirectReply();
        }
        event.preventDefault();
    } else if (event.code === "KeyD") {
        conn.sendDelete(cursor.path);
        event.preventDefault();
    } else if (event.code === "KeyA") {
        conn.sendAct(cursor.path);
        event.preventDefault();
    }
});
