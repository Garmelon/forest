# API

The Forest API uses json over websockets as its packet format. The API is
intentionally kept simple so new clients are easy to create.

When the client or the server detects incorrect data being sent, it should close
the connection with a relevant error code (usually 1008) and the reason for the
disconnect in the close message.

## Opening handshake

When connection is established, the client must first send a
[client hello packet](#hello). Then the server must reply with a
[server hello packet](#hello-1). After this exchange, the client and server must
not send any more hello packets. They may send any other packets.

## Node

The client displays a tree of nodes to the user. The user can then interact with
that tree in various ways (see the [client packets](#client-packets)). The
tree's root is a list of top-level nodes. Those nodes then have lists of child
nodes.

A node is a JSON object with the following properties:

| Property   | Type          | Description                                          |
|------------|---------------|------------------------------------------------------|
| `id`       | string        | The node's ID                                        |
| `text`     | string        | The node's text contents                             |
| `children` | list of nodes | The node's child nodes                               |
| `edit`     | bool          | Whether the node's text can be edited                |
| `delete`   | bool          | Whether the node can be deleted                      |
| `reply`    | bool          | Whether a new child node to this node can be created |
| `act`      | bool          | Whether this node's action can be performed          |

Each node has an ID that is unique within the node list it is in. A node's ID
does not have to be unique globally.

When a node is displayed, its children should be displayed according to their
order in the list. The same is true for the root node list.

Here is an example node:

``` json
{
  "id": "node1",
  "text": "This is an example node",
  "edit": false,
  "delete": false,
  "reply": false,
  "act": false,
  "children": [
    {
      "id": "child1",
      "text": "And this is a child node",
      "edit": true,
      "delete": true,
      "reply": false,
      "act": false,
      "children": []
    },
    {
      "id": "child2",
      "text": "This is another child node",
      "edit": false,
      "delete": false,
      "reply": false,
      "act": true,
      "children": []
    }
  ]
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

### update

An `update` packet is sent by the server whenever the client's node tree
changes. When receiving an `update` package, the client should immediately
update its node tree and display the new tree.

| Property | Type          | Description                                            |
|----------|---------------|--------------------------------------------------------|
| `type`   | string        | The string `update`                                    |
| `path`   | path          | The path to the node whose children should be replaced |
| `nodes`  | list of nodes | The new list of children                               |

If the path is an empty path, the root node list should be replaced.

