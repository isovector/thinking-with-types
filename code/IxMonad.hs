-- # pragmas
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module IxMonad where

-- # imports
import Control.Monad.Indexed
import Data.Coerce

{-

-- # usage
{-# LANGUAGE RebindableSyntax #-}
import Language.Haskell.DoNotation
import Prelude hiding (Monad (..), pure)

-}


newtype Ix m i j a = Ix
  { unsafeRunIx :: m a
  }
  deriving (Functor, Applicative, Monad)


-- # IxFunctor
instance Functor m => IxFunctor (Ix m) where
  imap = fmap

-- # IxPointed
instance Applicative m => IxPointed (Ix m) where
  ireturn = pure

-- # IxApplicative
instance Applicative m => IxApplicative (Ix m) where
  iap
      :: forall i j k a b  -- ! 1
       . Ix m i j (a -> b)
      -> Ix m j k a
      -> Ix m i k b
  iap = coerce $ (<*>) @m @a @b  -- ! 2

-- # IxMonad
instance Monad m => IxMonad (Ix m) where
  ibind
      :: forall i j k a b
       . (a -> Ix m j k b)
      -> Ix m i j a
      -> Ix m i k b
  ibind = coerce $ (=<<) @m @a @b

{-

-- # classIxMonad
class IxApplicative m => IxMonad m where
  ibind :: (a -> m j k b) -> m i j a -> m i k b

-}

