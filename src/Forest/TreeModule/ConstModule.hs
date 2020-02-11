{-# LANGUAGE OverloadedStrings #-}

module Forest.TreeModule.ConstModule
  ( constModule
  , projectDescriptionNode
  ) where

import           Forest.Node
import           Forest.TreeModule

data ConstModule = ConstModule

instance TreeModule ConstModule where
  edit _ _ _ = pure ()
  delete _ _ = pure ()
  reply _ _ _ = pure ()
  act _ _ = pure ()

constModule :: Node -> ModuleConstructor ConstModule
constModule node sendNode continue = do
  sendNode node
  continue ConstModule

projectDescriptionNode :: Node
projectDescriptionNode =
  newNode "" "Hello."
  [ newNode "" "This project is an experiment in tree-based interaction." []
  , newNode "" "Its basic unit is the node (you're looking at one right now)." []
  , newNode "" "A node mostly contains text\nthat is not constrained\nto a\nsingle\nline." []
  , newNode "" "In addition to that, a node can have any number of child nodes."
    [ newNode "" "This node is an example child node." []
    , newNode "" "Of course, children can have children too."
      [ newNode "" "It's nodes all the way down."
        [ newNode "" "Just kidding, it's turtles of course." []
        ]
      ]
    ]
  , newNode "" "But that's not all there is to nodes." []
  , newNode "" "Each node also has a set of four different permissions."
    [ newNode "" "Those permissions describe how you can interact with a node (besides folding and unfolding it)." []
    , newNode "" "Here are the four permissions, by name:"
      [ newNode "e" "edit"
        [ newNode "" "If a node has the 'edit' permission, its text can be edited." []
        ]
      , newNode "d" "delete"
        [ newNode "" "If a node has the 'delete' permission, it can be deleted." []
        ]
      , newNode "r" "reply"
        [ newNode "" "If a node has the 'reply' permission, a child node with custom text can be created." []
        ]
      , newNode "a" "act"
        [ newNode "" "If a node has the 'act' permission, a node-specific action can be performed with it." []
        ]
      , newNode "" "The above nodes have their respective permission set, but the server will ignore any actions." []
      , newNode "" "Feel free to try out all the permissions, but don't be surprised if you try to delete a node and nothing happens." []
      ]
    , newNode "" "Of course, a single node can have any combination of permissions."
      [ newNode "er" "For example, this node has both the 'edit' and 'reply' permissions." []
      , newNode "eda" "And this node has all permissions except the 'reply' permission." []
      ]
    ]
  ]
