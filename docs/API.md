# API

The Forest API uses json over websockets as its packet format. The API is
intentionally kept simple so new clients are easy to create.

When the client or the server detects incorrect data being sent, it should close
the connection with a relevant
[status code](https://tools.ietf.org/html/rfc6455#section-7.4) (usually 1008)
and the reason for the disconnect in the close message.

## Opening handshake

When connection is established, the client must first send a
[client hello packet](#hello). Then the server must reply with a
[server hello packet](#hello-1). After this exchange, the client and server must
not send any more hello packets. They may send any other packets.

## Node

The client displays a tree of nodes to the user. The user can then interact with
that tree in various ways (see the [client packets](#client-packets)). The
tree's root is a single node. This node has child nodes which then have child
nodes themselves and so on.

A node is a JSON object with the following properties:

| Property   | Type                     | Description                                          |
|------------|--------------------------|------------------------------------------------------|
| `text`     | string                   | The node's text contents                             |
| `edit`     | bool                     | Whether the node's text can be edited                |
| `delete`   | bool                     | Whether the node can be deleted                      |
| `reply`    | bool                     | Whether a new child node to this node can be created |
| `act`      | bool                     | Whether this node's action can be performed          |
| `children` | map from node ID to node | The node's child nodes                               |
| `order`    | list of node IDs         | The order the children should be displayed in        |

Each node has a map of child nodes which are identified by an ID. That ID does
not have to be unique globally.

In addition to that, each node also defines a display order for its child nodes
in the form of a list of node IDs. The list contains each child node ID exactly
once. The nodes are listed from top to bottom.

Here is an example node:

``` json
{
  "text": "This is an example node",
  "edit": false,
  "delete": false,
  "reply": false,
  "act": false,
  "children": {
    "child1": {
      "text": "And this is a child node",
      "edit": true,
      "delete": true,
      "reply": false,
      "act": false,
      "children": {},
      "order": []
    },
    "child2": {
      "text": "This is another child node",
      "edit": false,
      "delete": false,
      "reply": false,
      "act": true,
      "children": {},
      "order": []
    }
  },
  "order": ["child2", "child1"]
}
```

## Path

A path is a list of node IDs that describes a single node, similar to how
filesystem paths use directory and file names to describe a single directory or
file.

Here is an example path that points to a node with ID `child2`, which is the
child node of a node with ID `node1`:

``` json
["node1", "child2"]
```

## Client packets

### hello

A `hello` packet is the first packet the client sends when connecting to a
server. It can be used to request protocol extensions.

| Property     | Type            | Description                                               |
|--------------|-----------------|-----------------------------------------------------------|
| `type`       | string          | The string `hello`                                        |
| `extensions` | list of strings | The protocol extensions the client requests to be enabled |

### edit

An `edit` packet can be sent to modify a node's text. It should only be sent if
the node's `edit` field is set to `true`.

| Property | Type   | Description                   |
|----------|--------|-------------------------------|
| `type`   | string | The string `edit`             |
| `path`   | path   | The node whose text to modify |
| `text`   | string | The new text                  |

### delete

A `delete` packet can be sent to delete a node. It should only be sent if the
node's `delete` field is set to `true`.

| Property | Type   | Description         |
|----------|--------|---------------------|
| `type`   | string | The string `delete` |
| `path`   | path   | The node to delete  |

### reply

A `reply` packet can be sent to add a new child node to a node. It should only
be sent if the node's `reply` field is set to `true`.

| Property | Type   | Description                         |
|----------|--------|-------------------------------------|
| `type`   | string | The string `reply`                  |
| `path`   | path   | The node to add a new child node to |
| `text`   | string | The text of the child node          |

### act

An `act` packet can be sent to perform a node-specific action. It should only be
sent if the node's `act` field is set to `true`.

| Property | Type   | Description                      |
|----------|--------|----------------------------------|
| `type`   | string | The string `act`                 |
| `path`   | path   | The node whose action to perform |

## Server packets

### hello

A `hello` packet is the first packet the server sends when a new client
connects. It is sent in reply to the [client's hello packet](#hello) and
contains the protocol extensions that will be active for this connection.

| Property     | Type            | Description                                                     |
|--------------|-----------------|-----------------------------------------------------------------|
| `type`       | string          | The string `hello`                                              |
| `extensions` | list of strings | The protocol extensions that will be active for this connection |
| `node`       | node            | The initial root node                                           |

### update

An `update` packet is sent by the server whenever the client's node tree
changes. When receiving an `update` packet, the client should immediately update
its node tree and display the new tree.

| Property | Type   | Description                                  |
|----------|--------|----------------------------------------------|
| `type`   | string | The string `update`                          |
| `path`   | path   | The path to the node that should be replaced |
| `node`   | node   | The replacement node                         |
