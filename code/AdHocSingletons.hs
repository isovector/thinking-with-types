-- # pragmas
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module AdHocSingletons where

-- # imports
import Control.Monad.Trans.Writer
import Data.Constraint (Dict (..))
import Data.Foldable (for_)
import Data.Kind (Type)

data SomeSBool where
  SomeSBool :: SBool b -> SomeSBool

data SBool (b :: Bool) where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

toSBool :: Bool -> SomeSBool
toSBool True  = SomeSBool STrue
toSBool False = SomeSBool SFalse

fromSBool :: SBool b -> Bool
fromSBool STrue  = True
fromSBool SFalse = False

withSomeSBool
    :: SomeSBool
    -> (forall (b :: Bool). SBool b -> r)
    -> r
withSomeSBool (SomeSBool s) f = f s

type family EnableLogging (b :: Bool) :: Type -> Type where
  EnableLogging 'True  = WriterT [String] IO
  EnableLogging 'False = IO

-- # MonadLogging
class Monad (LoggingMonad b)
      => MonadLogging (b :: Bool) where
  type LoggingMonad b = (r :: Type -> Type)  -- ! 1
         | r -> b  -- ! 2
  logMsg :: String -> LoggingMonad b ()
  runLogging :: LoggingMonad b a -> IO a

-- # MonadLoggingTrue
instance MonadLogging 'True where
  type LoggingMonad 'True = WriterT [String] IO
  logMsg s = tell [s]
  runLogging m = do
    (a, w) <- runWriterT m
    for_ w putStrLn
    pure a

-- # MonadLoggingFalse
instance MonadLogging 'False where
  type LoggingMonad 'False = IO
  logMsg _ = pure ()
  runLogging = id

dict
  :: ( c 'True  -- ! 1
     , c 'False
     )
  => SBool b  -- ! 2
  -> Dict (c b)
dict STrue  = Dict  -- ! 3
dict SFalse = Dict

program :: MonadLogging b => LoggingMonad b ()
program = do
  logMsg "hello world"
  pure ()

main :: Bool -> IO ()
main bool = do
  withSomeSBool (toSBool bool) $ \(sb :: SBool b) ->
    case dict @MonadLogging sb of
      Dict -> runLogging @b program

{-

-- # badMain
main :: IO ()
main = do
  bool <- read <$> getLine
  withSomeSBool (toSBool bool) $  -- ! 1
    \(_ :: SBool b) ->  -- ! 2
      runLogging @b program  -- ! 3

-}

