module Forest.Client.Options
  ( ClientOptions(..)
  , clientOptionsParserInfo
  ) where

import           Data.List
import           Options.Applicative
import           Options.Applicative.Help.Pretty

data ClientOptions = ClientOptions
  { clientHostName :: String
  , clientPort     :: Int
  , clientPath     :: String
  , clientSsl      :: Bool
  }

parser :: Parser ClientOptions
parser = ClientOptions
  <$> strArgument
      (  help "The name of the host to connect to"
      <> metavar "HOST"
      )
  <*> option auto
      (  short 'p'
      <> long "port"
      <> help "The port to connect to"
      <> value 11133 -- Chosen by fair dice roll
      <> showDefault
      <> metavar "PORT"
      )
  <*> strOption
      (  short 'P'
      <> long "path"
      <> help "The path to connect to on the given domain"
      <> value ""
      <> showDefault
      <> metavar "PATH"
      )
  <*> flag True False -- Ssl enabled by default
      (  short 'n'
      <> long "no-ssl"
      <> help "This flag disables ssl on outgoing websocket connections"
      )

keyBindings :: String
keyBindings = intercalate "\n"
  [ "Key bindings:"
  , "  exit                     q, esc"
  , "  move cursor              up/down, j/k"
  , "  toggle fold              tab"
  , "  edit node                e"
  , "  delete node              d"
  , "  new child (reply)        r"
  , "  new sibling              R"
  , "  perform action           a, enter, space"
  , ""
  , "Editor key bindings:"
  , "  confirm edit             enter"
  , "  abort edit               esc"
  , "  insert newline           ctrl+n"
  ]

clientOptionsParserInfo :: ParserInfo ClientOptions
clientOptionsParserInfo = info (helper <*> parser)
  (  fullDesc
  <> footerDoc (Just $ string keyBindings)
  )
