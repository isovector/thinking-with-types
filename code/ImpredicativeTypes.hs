{-# LANGUAGE ImpredicativeTypes #-}

{-# LANGUAGE NumDecimals        #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module ImpredicativeTypes where

import Control.Concurrent
import Control.Exception (bracket_)
import Control.Monad (void)


makeSerial :: IO (forall a. IO a -> IO a)
makeSerial = fmap locking newEmptyMVar


locking :: MVar () -> (forall a. IO a -> IO a)
locking lock =
  bracket_
    (putMVar lock ())
    (takeMVar lock)

dump :: (IO () -> IO ()) -> IO ()
dump f =
    void $ forkIO $ do
      f $ putStrLn "hello"
      f $ putStrLn "hello"

serialized :: IO ()
serialized = do
  makeSerial >>= \serial -> do  -- ! 1
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

