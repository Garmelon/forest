{-# LANGUAGE OverloadedStrings #-}

-- | This module contains all the types found in the API.

module Forest.Api
  (
  -- * Common
    NodeId
  , Node(..)
  , Path(..)
  -- * Client
  , ClientPacket(..)
  -- * Server
  , ServerPacket(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T

{- Common -}

type NodeId = T.Text

data Node = Node
  { nodeText     :: T.Text
  , nodeAct      :: Bool
  , nodeEdit     :: Bool
  , nodeDelete   :: Bool
  , nodeReply    :: Bool
  , nodeChildren :: Map.HashMap NodeId Node
  } deriving (Show)

instance ToJSON Node where
  toJSON node = object
    [ "text"     .= nodeText node
    , "act"      .= nodeAct node
    , "edit"     .= nodeEdit node
    , "delete"   .= nodeDelete node
    , "reply"    .= nodeReply node
    , "children" .= nodeChildren node
    ]

  toEncoding node = pairs
    (  "text"     .= nodeText node
    <> "act"      .= nodeAct node
    <> "edit"     .= nodeEdit node
    <> "delete"   .= nodeDelete node
    <> "reply"    .= nodeReply node
    <> "children" .= nodeChildren node
    )

instance FromJSON Node where
  parseJSON v = parseJSON v >>= \o -> Node
    <$> o .: "text"
    <*> o .: "act"
    <*> o .: "edit"
    <*> o .: "delete"
    <*> o .: "reply"
    <*> o .: "children"

newtype Path = Path
  { pathElements :: [NodeId]
  } deriving (Show, Eq)

instance ToJSON Path where
  toJSON = toJSON . pathElements
  toEncoding = toEncoding . pathElements

instance FromJSON Path where
  parseJSON v = Path <$> parseJSON v

parsePacket :: Value -> T.Text -> (Object -> Parser a) -> Parser a
parsePacket value packetType parser = parseJSON value >>= \o -> do
  parsedPacketType <- o .: "type"
  guard $ parsedPacketType == packetType
  parser o

{- Client -}

data ClientPacket
  = ClientHello [T.Text]
  | ClientAct Path
  | ClientEdit Path T.Text
  | ClientDelete Path
  | ClientReply Path T.Text
  deriving (Show)

instance ToJSON ClientPacket where
  toJSON (ClientHello extensions) =
    object ["type" .= ("hello" :: T.Text), "extensions" .= extensions]
  toJSON (ClientAct path) = object ["type" .= ("act" :: T.Text), "path" .= path]
  toJSON (ClientEdit path text) =
    object ["type" .= ("edit" :: T.Text), "path" .= path, "text" .= text]
  toJSON (ClientDelete path) =
    object ["type" .= ("delete" :: T.Text), "path" .= path]
  toJSON (ClientReply path text) =
    object ["type" .= ("reply" :: T.Text), "path" .= path, "text" .= text]

  toEncoding (ClientHello extensions) =
    pairs ("type" .= ("hello" :: T.Text) <> "extensions" .= extensions)
  toEncoding (ClientAct path) =
    pairs ("type" .= ("act" :: T.Text) <> "path" .= path)
  toEncoding (ClientEdit path text) =
    pairs ("type" .= ("edit" :: T.Text) <> "path" .= path <> "text" .= text)
  toEncoding (ClientDelete path) =
    pairs ("type" .= ("delete" :: T.Text) <> "path" .= path)
  toEncoding (ClientReply path text) =
    pairs ("type" .= ("reply" :: T.Text) <> "path" .= path <> "text" .= text)

instance FromJSON ClientPacket where
  parseJSON v =
    parsePacket v "hello"  (\o -> ClientHello  <$> o .: "extensions")           <|>
    parsePacket v "act"    (\o -> ClientAct    <$> o .: "path")                 <|>
    parsePacket v "edit"   (\o -> ClientEdit   <$> o .: "path" <*> o .: "text") <|>
    parsePacket v "delete" (\o -> ClientDelete <$> o .: "path")                 <|>
    parsePacket v "reply"  (\o -> ClientReply  <$> o .: "path" <*> o .: "text")

{- Server -}

data ServerPacket
  = ServerHello [T.Text]
  | ServerUpdate Path Node
  deriving (Show)

instance ToJSON ServerPacket where
  toJSON (ServerHello extensions) =
    object ["type" .= ("hello" :: T.Text), "extensions" .= extensions]
  toJSON (ServerUpdate path node) =
    object ["type" .= ("update" :: T.Text), "path" .= path, "node" .= node]

  toEncoding (ServerHello extensions) =
    pairs ("type" .= ("hello" :: T.Text) <> "extensions" .= extensions)
  toEncoding (ServerUpdate path node) =
    pairs ("type" .= ("update" :: T.Text) <> "path" .= path <> "node" .= node)

instance FromJSON ServerPacket where
  parseJSON v =
    parsePacket v "hello" (\o -> ServerHello <$> o .: "extensions") <|>
    parsePacket v "update" (\o -> ServerUpdate <$> o .: "path" <*> o .: "node")
