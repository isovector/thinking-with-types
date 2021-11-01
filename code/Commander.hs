{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Commander where

type Point = (Int, Int)
type Guy = String


data Command f where
  InstantCmd
     :: (IsCommand a, IsInstantCmd a)
     => f a ()
     -> Command f
  GuyCmd
      :: (IsCommand a, IsGuyCmd a)
      => f a Guy
      -> Command f
  LocationCmd
      :: (IsCommand a, IsLocationCmd a)
      => f a Point
      -> Command f


class IsCommand a where
  runCommand :: a -> IO (Maybe a)


class IsInstantCmd a where
  fromInstant :: IO a

class IsGuyCmd a where
  fromGuy :: Guy -> IO a

class IsLocationCmd a where
  fromLocation :: Point -> IO a


data Proxy2 a b = Proxy2
type Prototype f = f Proxy2


data Callback a b = Callback
  { runCallback :: b -> IO a
  }
type Pending f = f Callback


issue :: Prototype Command -> Pending Command
issue (InstantCmd (Proxy2 :: Proxy2 a ())) =
  InstantCmd $ Callback $ const $ fromInstant @a
issue (GuyCmd (Proxy2 :: Proxy2 a Guy)) =
  GuyCmd $ Callback $ fromGuy @a
issue (LocationCmd (Proxy2 :: Proxy2 a Point)) =
  LocationCmd $ Callback $ fromLocation @a


data RunningCommand where
  RunningCommand
      :: (IsCommand a)
      => a
      -> RunningCommand


start :: Pending Command -> IO (RunningCommand)
start = \case
  InstantCmd cb ->
    RunningCommand <$> runCallback cb ()
  GuyCmd cb -> do
    putStrLn "which guy my dude?"
    guy <- read <$> getLine
    RunningCommand <$> runCallback cb guy
  LocationCmd cb -> do
    putStrLn "which loc my lord?"
    loc <- read <$> getLine
    RunningCommand <$> runCallback cb loc


pump :: RunningCommand -> IO (Maybe RunningCommand)
pump (RunningCommand cmd) = do
  mcmd' <- runCommand cmd
  pure $ fmap RunningCommand mcmd'

