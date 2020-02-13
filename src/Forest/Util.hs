{-# LANGUAGE OverloadedStrings #-}

module Forest.Util
  ( findPrev
  , findNext
  , whileM
  , runUntilJustM
  , withThread
  , sendPacket
  , closeWithErrorMessage
  , receivePacket
  ) where

import           Control.Concurrent.Async
import           Control.Monad
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

-- | Run a monadic action until it returns @False@ for the first time.
whileM :: Monad m => m Bool -> m ()
whileM f = do
  continue <- f
  if continue
    then whileM f
    else pure ()

-- | Run a monadic action until it returns @Just a@ for the first time.
runUntilJustM :: Monad m => m (Maybe a) -> m a
runUntilJustM f = do
  result <- f
  case result of
    Nothing -> runUntilJustM f
    Just a  -> pure a

sendPacket :: ToJSON a => WS.Connection -> a -> IO ()
sendPacket conn packet = WS.sendTextData conn $ encode packet

waitForCloseException :: WS.Connection -> IO a
waitForCloseException conn = forever $ void $ WS.receiveDataMessage conn

closeWithErrorMessage :: WS.Connection -> T.Text -> IO a
closeWithErrorMessage conn text =
  WS.sendCloseCode conn 1003 text >> waitForCloseException conn

receivePacket :: FromJSON a => WS.Connection -> IO a
receivePacket conn = do
  dataMessage <- WS.receiveDataMessage conn
  closeOnErrorMessage $ case dataMessage of
    WS.Binary _  -> Left "Invalid message format: Binary"
    WS.Text bs _ -> case eitherDecode' bs of
      Left errorMsg -> Left $ "Invalid packet: " <> T.pack errorMsg
      Right packet  -> Right packet
  where
    closeOnErrorMessage :: Either T.Text a -> IO a
    closeOnErrorMessage (Right a)       = pure a
    closeOnErrorMessage (Left errorMsg) = closeWithErrorMessage conn errorMsg
