-- | A 'Broadcaster' allows threads to 'broadcast' values to 'Listeners'
-- attached to that broadcaster. A value that is sent through a broadcaster will
-- arrive exactly once at each attached listener and can then be collected by
-- calling 'listen'.
--
-- All functions included in this module should be threadsafe. Be sure to read
-- the warning on the 'broadcast' function.

module Forest.Server.Broadcast
  ( Broadcaster
  , Listener
  , newBroadcaster
  , attachListener
  , broadcast
  , listen
  ) where

import           Control.Concurrent.Chan

-- | A 'Broadcaster' can broadcast values to all attached 'Listener's
newtype Broadcaster a = Broadcaster (Chan a)

-- | A 'Listener' receives values from the 'Broadcaster' it is attached to
newtype Listener a = Listener (Chan a)

-- | Create a new 'Broadcaster'
newBroadcaster :: IO (Broadcaster a)
newBroadcaster = Broadcaster <$> newChan

-- | Create a new 'Listener' that is attached to a 'Broadcaster'
attachListener :: Broadcaster a -> IO (Listener a)
attachListener (Broadcaster chan) = Listener <$> dupChan chan

-- | Send a value through the 'Broadcaster'. That value will arrive exactly once
-- at all 'Listener's attached to this broadcaster via 'attachListener'.
--
-- Warning: During this function call, no exception should occur or elements may
-- build up in the broadcaster, leading to a memory/space leak.
broadcast :: Broadcaster a -> a -> IO ()
-- Because the same function that puts something into the broadcaster channel
-- also immediately reads something from that channel, there is no build-up of
-- values in the broadcaster channel, as one element is removed for each element
-- written. Since the broadcaster channel is separate from the listener
-- channels, no event is swallowed accidentally.
--
-- If some exception happens after the write operation succeeds but before the
-- read operation finishes, elements can build up in the broadcast channel.
broadcast (Broadcaster chan) value = writeChan chan value <* readChan chan

-- | Read the next value from the 'Listener'. Blocks when the listener is empty.
listen :: Listener a -> IO a
listen (Listener chan) = readChan chan
