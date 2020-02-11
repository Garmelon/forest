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
  newNode "" "About"
  [ newNode "" "This project is an experiment in tree-based interaction." []
  , newNode "" "Motivation"
    [ newNode "" "My goals for this project were:" []
    , newNode "" "Interactons between multiple people"
      [ newNode "" "I wanted to create a project that let multiple people interact with each other in different ways." []
      , newNode "" "Examples for interactions include:" []
      , newNode "" "Chatting" []
      , newNode "" "Collaborative editing" []
      , newNode "" "Playing (multiplayer) games" []
      , newNode "" "The project should allow for many different kinds of interactions." []
      ]
    , newNode "" "Portability"
      [ newNode "" "The project should be usable on multiple different platforms." []
      , newNode "" "To facilitate this, clients should be easy to create." []
      , newNode "" "In particular, I wanted at least one terminal-based and one web-based client." []
      ]
    , newNode "" "Based on these goals, I made the following design decisions:" []
    , newNode "" "Text-based"
      [ newNode "" "Text is the one medium that works on all platforms." []
      , newNode "" "Text is also easy to work with as a programmer." []
      , newNode "" "Finally, text still allows for a lot of different interactions." []
      , newNode "" "Of all the kinds of media one can produce with a computer, text is easy and quick to create." []
      , newNode "" "After all, pretty much every computer has a keyboard." []
      ]
    , newNode "" "Tree-based"
      [ newNode "" "While plain text may be easy to work with, it makes interactions cumbersome if limited to basic input and output." []
      , newNode "" "To make interactions nicer, the server could send the client a screen's worth of text to display, in effect creating a TUI-like interface." []
      , newNode "" "The client would then only need to send key presses or mouse clicks to the server." []
      , newNode "" "In my opinion, that approach moves too many decisions on how to interact to the server and imposes unnecessary limits on the client design." []
      , newNode "" "Instead, I went with a plaintext-in-tree-structure approach, which allows for more flexibility in the client design." []
      , newNode "" "Also, this might make bots easier to write, since they don't have to emulate human input." []
      ]
    , newNode "" "Simple API"
      [ newNode "" "Every client must use the same API to interact with the server." []
      , newNode "" "Because clients should be easy to create on different platforms, the API should also be simple." []
      , newNode "" "One way in which the API is simple is that the server doesn't send direct responses to client commands." []
      , newNode "" "Instead, there is only the 'update' packet, which is sent whenever the client should modify its tree structure." []
      , newNode "" "In total, there are 5 different client packages and 2 different server packages." []
      , newNode "" "If at some point the API turns out to be too simple, it has a built-in way of negotiating protocol extensions." []
      ]
    , newNode "" "Most logic in server"
      [ newNode "" "All logic besides the immediate input handling and tree folding happens in the server." []
      , newNode "" "This has multiple advantages:" []
      , newNode "" "The API and clients are simpler, clients are easier to write or maintain." []
      , newNode "" "Updates in logic don't require updates of the client." []
      , newNode "" "The server-side logic becomes easier to write." []
      ]
    , newNode "" "Those design decisions should allow for various different kinds of interactions." []
    , newNode "" "For example: Linear and threaded chat, collaborative node editing, reading node-based documents (like this one), playing text adventures and more." []
    , newNode "" "And of course, which interactions are supported only depends on the server and not on the client." []
    ]
  , newNode "" "Inspirations"
    [ newNode "" "The tree-based chat model and UI of euphoria (euphoria.io) and instant (instant.leet.nu)" []
    , newNode "" "MUDs (which are text based and most of the logic happens server-side)" []
    ]
  ]
