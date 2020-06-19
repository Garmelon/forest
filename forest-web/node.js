"use strict";

/*
 * Utility functions
 */

function removeAllChildren(element) {
    while (element.firstChild) {
        element.removeChild(element.lastChild);
    }
}

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
    constructor(...components) {
        this._components = components.slice();
    }

    get components() {
        return this._components.slice();
    }

    get length() {
        return this._components.length;
    }

    get last() {
        return this._components[this.length - 1];
    }

    get parent() {
        if (this.length === 0) return undefined;
        return new Path(...this._components.slice(0, this.length - 1));
    }

    append(nodeId) {
        return new Path(...this._components.concat([nodeId]));
    }

    concat(otherPath) {
        return new Path(...this._components.concat(otherPath._components));
    }
}

class NodeElements {
    constructor() {
        this._elText = newElement("span", "node-text");
        this._elPermissions = newElement("span", "node-permissions");
        this._elChildren = newElement("div", "node-children");

        let line = newElement("div", "node-line", this._elText, this._elPermissions);
        this._elMain = newElement("div", ["node", "is-folded"], line, this._elChildren);
    }

    get text() {
        return this._elText.textContent;
    }

    set text(text) {
        this._elText.textContent = text;
    }

    set permissions(perms) {
        this._elPermissions.textContent = perms.asText;
    }

    get hasChildren() {
        return this._elMain.classList.contains("has-children");
    }

    set hasChildren(flag) {
        return this._elMain.classList.toggle("has-children", flag);
    }

    removeAllChildren() {
        removeAllChildren(this._elChildren);
    }

    addChild(child) {
        this._elChildren.appendChild(child._elMain);
    }

    appendTo(element) {
        element.appendChild(this._elMain);
    }

    get folded() {
        return this._elMain.classList.contains("is-folded");
    }

    set folded(flag) {
        this._elMain.classList.toggle("is-folded", flag);
    }

    toggleFolded() {
        this.folded = !this.folded;
    }

    get hasCursor() {
        return this._elMain.classList.contains("has-cursor");
    }

    set hasCursor(flag) {
        return this._elMain.classList.toggle("has-cursor", flag);
    }

    get hasEditor() {
        return this._elMain.classList.contains("has-editor");
    }

    set hasEditor(flag) {
        return this._elMain.classList.toggle("has-editor", flag);
    }
}

class NodePermissions {
    constructor(edit, delete_, reply, act) {
        this._edit = edit;
        this._delete = delete_;
        this._reply = reply;
        this._act = act;
    }

    get edit() {
        return this._edit;
    }

    get delete() {
        return this._delete;
    }

    get reply() {
        return this._reply;
    }

    get act() {
        return this._act;
    }

    get asText() {
        return [
            "(",
            this.edit   ? "e" : "-",
            this.delete ? "d" : "-",
            this.reply  ? "r" : "-",
            this.act    ? "a" : "-",
            ")"
        ].join("");
    }
}

class Node {
    constructor(nodeJson) {
        this._el = undefined;

        this._text = nodeJson.text;

        this._permissions = new NodePermissions(
            nodeJson.edit,
            nodeJson.delete,
            nodeJson.reply,
            nodeJson.act,
        );

        this._children = new Map();
        this._order = nodeJson.order;
        this._order.forEach(childId => {
            let childJson = nodeJson.children[childId];
            let childNode = new Node(childJson);
            this._children.set(childId, childNode);
        });
    }

    child(childId) {
        return this._children.get(childId);
    }

    get order() {
        return this._order.slice();
    }

    // Only replaces existing children. Does not add new children.
    replaceChild(childId, newChild) {
        let oldChild = this.child(childId);
        if (oldChild === undefined) return;
        newChild.obtainElements(oldChild);
        this._children.set(childId, newChild);
    }

    // Obtain and update this node's DOM elements. After this call, this.el
    // represents the current node's contents.
    //
    // This function may optionally be called with an old node. If that node or
    // its children already has existing DOM elements, they are repurposed.
    // Otherwise, new DOM elements are created.
    obtainElements(oldNode) {
        if (this._el === undefined) {
            // Obtain DOM elements because we don't yet have any
            if (oldNode === undefined || oldNode._el === undefined) {
                this._el = new NodeElements();
            } else {
                this._el = oldNode._el;
            }
        }

        this._el.text = this._text;
        this._el.permissions = this._permissions;
        this._el.hasChildren = this.order.length > 0;

        this._el.removeAllChildren();

        let oldChildren = (oldNode === undefined) ? new Map() : oldNode._children;
        this._order.forEach(childId => {
            let oldChild = oldChildren.get(childId); // May be undefined
            let child = this._children.get(childId); // Not undefined
            child.obtainElements(oldChild);
            this._el.addChild(child._el);
        });
    }

    // Wrapper functions for this._el

    appendTo(element) {
        if (this._el === undefined) this.obtainElements();
        this._el.appendTo(element);
    }

    get folded() {
        if (this._el === undefined) return undefined;
        return this._el.folded;
    }

    set folded(flag) {
        if (this._el === undefined) return;
        this._el.folded = flag;
    }

    toggleFolded() {
        if (this._el === undefined) return;
        this._el.toggleFolded();
    }

    get hasCursor() {
        if (this._el === undefined) return undefined;
        return this._el.hasCursor;
    }

    set hasCursor(flag) {
        if (this._el === undefined) return;
        this._el.hasCursor = flag;
    }

    get hasEditor() {
        if (this._el === undefined) return undefined;
        return this._el.hasEditor;
    }

    set hasEditor(flag) {
        if (this._el === undefined) return;
        this._el.hasEditor = flag;
    }
}

class NodeTree {
    constructor(rootNodeContainer, rootNode) {
        this._rootNodeContainer = rootNodeContainer;
        this._rootNode = rootNode;

        // Prepare root node container
        removeAllChildren(this._rootNodeContainer);
        this._rootNode.appendTo(this._rootNodeContainer);
    }

    at(path) {
        let node = this._rootNode;
        for (let childId of path.components) {
            node = node.child(childId);
            if (node === undefined) break;
        }
        return node;
    }

    updateAt(path, newNode) {
        if (path.length === 0) {
            newNode.obtainElements(this._rootNode);
            this._rootNode = newNode;
        } else {
            let parentNode = this.at(path.parent);
            parentNode.replaceChild(path.last, newNode);
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
            if (prevNode.folded) return prevPath;

            let childPath = this.getLastChild(prevPath);
            if (childPath === undefined) return prevPath;

            prevPath = childPath;
        }
    }

    getNodeBelow(path) {
        let node = this.at(path);
        if (!node.folded) {
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
        this._nodeTree = nodeTree;

        this._path = new Path();
        this._relPos = null; // Either null or a RelPos value

        this.restore();
    }

    getSelectedNode() {
        return this._nodeTree.at(this._path);
    }

    _applyRelPos() {
        if (this._relPos === null) return;

        let newPath;
        if (this._relPos === RelPos.FIRST_CHILD) {
            newPath = this._nodeTree.getFirstChild(this._path);
        } else if (this._relPos === RelPos.NEXT_SIBLING) {
            newPath = this._nodeTree.getNextSibling(this._path);
        }

        if (newPath !== undefined) {
            this._path = newPath;
            this._relPos = null;
        }
    }

    _moveToNearestValidNode() {
        // TODO Maybe select a sibling instead of going to nearest visible parent
        let path = new Path();
        for (let component of this._path.components) {
            let newPath = path.append(component);
            let newNode = this._nodeTree.at(newPath);
            if (newNode === undefined) break;
            if (newNode.folded) break;
            path = newPath;
        }
        this._path = path;
    }

    _set(visible) {
        this.getSelectedNode().hasCursor = visible;
    }

    restore() {
        this._applyRelPos();
        this._moveToNearestValidNode();
        this._set(true);
    }

    moveTo(path) {
        if (path === undefined) return;
        this._set(false);
        this._path = path;
        this._set(true);
    }

    moveUp() {
        this.moveTo(this._nodeTree.getNodeAbove(this._path));
    }

    moveDown() {
        this.moveTo(this._nodeTree.getNodeBelow(this._path));
    }
}

class Editor {
    constructor(nodeTree) {
        this._nodeTree = nodeTree;

        this._elTextarea = newElement("textarea");
        this._elTextarea.addEventListener("input", event => this._updateTextAreaHeight());
        this._elMain = newElement("div", "node-editor", this.textarea);

        this._path = undefined;
        this._asChild = false;
    }

    _updateTextAreaHeight() {
        this._elTextarea.style.height = 0;
        this._elTextarea.style.height = this._elTextarea.scrollHeight + "px";
    }

    _getAttachedNode() {
        if (this._path === undefined) return undefined;
        return this._nodeTree.at(this._path);
    }

    _detach(node, asChild) {
        if (!asChild) {
            node.hasEditor = false;
        }

        this._elMain.parentNode.removeChild(this._elMain);
    }

    _attachTo(node, asChild) {
        if (asChild) {
            node._el._elChildren.appendChild(this.element);
            node.folded = false;
        } else {
            node._el._elMain.classList.add("has-editor");
            node._el._elMain.insertBefore(this.element, node._el._elChildren);
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
        this._send({type: "edit", path: path.components, text: text});
    }

    sendDelete(path) {
        this._send({type: "delete", path: path.components});
    }

    sendReply(path, text) {
        this._send({type: "reply", path: path.components, text: text});
    }

    sendAct(path) {
        this._send({type: "act", path: path.components});
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
