{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NumDecimals        #-}

module ImpredicativeTypes where

import Control.Concurrent
import Control.Exception (bracket_)
import Data.Foldable (for_)
import Control.Monad (void)

-- | Make a locking serializer
makeSerial :: IO (forall a. IO a -> IO a)
makeSerial = fmap locking newEmptyMVar

-- | Make a locking serializer with a given lock
locking :: MVar () -> (forall a. IO a -> IO a)
locking lock = bracket_ (putMVar lock ()) (takeMVar lock)

dump :: (IO () -> IO ()) -> IO ()
dump f =
    void $ forkIO $
      for_ [0..2] $ const @_ @Int $ f $ putStrLn "hello"

serialized :: IO ()
serialized = do
  makeSerial >>= \serial -> do
    dump serial
    dump serial

  threadDelay 1e5


interleaved :: IO ()
interleaved = do
  dump id
  dump id

  threadDelay 1e5

yo :: (forall a. [a] -> [a]) -> Int
yo _ = 0

($$) :: (a -> b) -> a -> b
g $$ a = g a

blah :: Int
blah = yo $$ reverse

