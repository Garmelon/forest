{-# LANGUAGE OverloadedStrings #-}

module Forest.TreeModule.ConstModule
  ( ConstModule
  , constModule
  , projectDescriptionNode
  ) where

import           Forest.Node
import           Forest.TreeModule

data ConstModule = ConstModule

instance TreeModule ConstModule where

constModule :: Node -> ModuleConstructor ConstModule
constModule node sendNode continue = do
  sendNode node
  continue ConstModule

projectDescriptionNode :: Node
projectDescriptionNode =
  newNode "" "About"
  [ txtNode "" "This project is an experiment in tree-based interaction."
  , newNode "" "Motivation"
    [ txtNode "" "My goals for this project were:"
    , newNode "" "Interactons between multiple people"
      [ txtNode ""
        (  "I wanted to create a project that let multiple people interact with "
        <> "each other in different ways. Examples for interactions include:\n"
        <> "* Chatting\n"
        <> "* Collaborative editing\n"
        <> "* Playing (multiplayer) games\n"
        )
      , txtNode "" "The project should allow for many different kinds of interactions."
      ]
    , newNode "" "Portability"
      [ txtNode ""
        (  "The project should be usable on multiple different platforms. To "
        <> "facilitate this, clients should be easy to create. In particular, I "
        <> "want at least one terminal-based and one web-based client."
        )
      ]
    , txtNode "" "Based on these goals, I made the following design decisions:"
    , newNode "" "Text-based"
      [ txtNode ""
        (  "Text is a medium that works on all platforms and easy to work with "
        <> "as a developer."
        )
      , txtNode ""
        (  "But text still allows for a lot of different interactions. Of all "
        <> "the kinds of media one can produce with a computer, text is easy "
        <> "and quick to create. After all, pretty much every computer has a "
        <> "keyboard."
        )
      ]
    , newNode "" "Tree-based"
      [ txtNode ""
        (  "While plain text may be easy to work with, it makes interactions "
        <> "cumbersome if limited to basic input and output. To make "
        <> "interactions nicer, the server could send the client a screen's "
        <> "worth of text to display, in effect creating a TUI-like interface. "
        <> "The client would then only need to send key presses or mouse clicks "
        <> "to the server."
        )
      , txtNode ""
        (  "In my opinion, that approach moves too many decisions on how to "
        <> "interact to the server and imposes unnecessary limits on the client "
        <> "design. Instead, I went with a plaintext-in-tree-structure "
        <> "approach, which allows for more flexibility in the client design. "
        <> "Also, this should make bots easier to write, since they don't have "
        <> "to emulate human input."
        )
      ]
    , newNode "" "Simple API"
      [ txtNode ""
        (  "Every client must use the same API to interact with the server. "
        <> "Because clients should be easy to create on different platforms, "
        <> "the API should also be simple."
        )
      , txtNode ""
        (  "One way in which the API is simple is that the server doesn't send "
        <> "direct responses to client commands. Instead, there is only the "
        <> "'update' packet, which is sent whenever the client should modify "
        <> "its tree structure."
        )
      , txtNode ""
        (  "In total, there are 5 different client packages and 2 different "
        <> "server packages. If at some point the API turns out to be too "
        <> "simple, it has a built-in way of negotiating protocol extensions."
        )
      ]
    , newNode "" "Most logic in server"
      [ txtNode ""
        (  "All logic besides the immediate input handling and tree folding "
        <> "happens in the server. This has multiple advantages:"
        )
      , txtNode "" "The API and clients are simpler, clients are easier to write or maintain."
      , txtNode "" "Updates in logic don't require updates of the client."
      , txtNode "" "The server-side logic becomes easier to write."
      ]
    , txtNode ""
      (  "Those design decisions should allow for various different kinds of "
      <> "interactions, for example linear and threaded chat, collaborative "
      <> "node editing, reading node-based documents (like this one), playing "
      <> "text adventures and more."
      )
    , txtNode ""
      (  "And of course, which interactions are supported only depends on the "
      <> "server and not on the client."
      )
    ]
  , newNode "" "Inspirations"
    [ txtNode "" "The tree-based chat model and UI of euphoria (euphoria.io) and instant (instant.leet.nu)"
    , txtNode "" "MUDs (which are text based and most of the logic happens server-side)"
    ]
  ]
