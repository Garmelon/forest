{-# LANGUAGE OverloadedStrings #-}

module Forest.Util
  ( findPrev
  , findNext
  , withThread
  , sendPacket
  , receivePacket
  , closeWithErrorMessage
  ) where

import           Control.Concurrent.Async
import           Data.Aeson
import           Data.List
import qualified Data.Text                as T
import qualified Network.WebSockets       as WS

findPrev :: (a -> Bool) -> [a] -> Maybe a
findPrev f as = fst <$> find (f . snd) (zip as $ tail as)

findNext :: (a -> Bool) -> [a] -> Maybe a
findNext f as = snd <$> find (f . fst) (zip as $ tail as)

withThread :: IO () -> IO () -> IO ()
withThread thread main = withAsync thread $ const main

sendPacket :: ToJSON a => WS.Connection -> a -> IO ()
sendPacket conn packet = WS.sendTextData conn $ encode packet

closeWithErrorMessage :: WS.Connection -> T.Text -> IO ()
closeWithErrorMessage conn = WS.sendCloseCode conn 1003

receivePacket :: FromJSON a => WS.Connection -> IO (Maybe a)
receivePacket conn = do
  dataMessage <- WS.receiveDataMessage conn
  closeOnErrorMessage $ case dataMessage of
    WS.Binary _  -> Left "Invalid message format: Binary"
    WS.Text bs _ -> case eitherDecode' bs of
      Left errorMsg -> Left $ "Invalid packet: " <> T.pack errorMsg
      Right packet  -> Right packet
  where
    closeOnErrorMessage :: Either T.Text a -> IO (Maybe a)
    closeOnErrorMessage (Right a) = pure $ Just a
    closeOnErrorMessage (Left errorMsg) =
      Nothing <$ closeWithErrorMessage conn errorMsg
