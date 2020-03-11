{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module QuantifiedConstraints where

import Data.Functor.Compose
import Data.Functor.Identity
import Data.Bifoldable
import Data.Monoid
import Control.Applicative
import Data.Bifunctor
import Data.Functor.Contravariant

class Profunctor p where
  lmap :: (a -> c) -> p c b -> p a b
  rmap :: (b -> c) -> p a b -> p a c

instance Profunctor (->) where
  lmap = undefined
  rmap = undefined


newtype AppNum f a = AppNum (f a)
  deriving stock Functor
  deriving newtype Applicative


instance (Applicative f, Num a)
    => Num (AppNum f a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs         = fmap abs
  signum      = fmap signum
  fromInteger = AppNum . pure . fromInteger


newtype Validation e a = Validation
  { toEither :: Either e a
  }
  deriving stock Functor
  deriving newtype Applicative
  deriving Num via AppNum (Either e) a
  -- deriving Applicative via _


data Pair f g a = Pair (f a) (g a)
  deriving stock Functor
  deriving Show

instance (Applicative f, Applicative g) => Applicative (Pair f g) where
  pure a = Pair (pure a) (pure a)
  Pair fa fb <*> Pair a b = Pair (fa <*> a) (fb <*> b)

newtype Triple a = Triple
  { getTriple :: Pair Identity (Pair Identity Identity) a
  }
  deriving stock (Functor, Show)
  deriving newtype Applicative


newtype Sudoku a = Sudoku
  { runSudoku :: Triple (Triple a)
  }
  deriving stock Functor
  deriving Applicative via Compose Triple Triple



newtype Flip p a b = Flip { runFlip :: p b a }

instance Profunctor p => Contravariant (Flip p a) where
  contramap f = Flip . lmap f . runFlip

instance Bifunctor p => Bifunctor (Flip p) where
  bimap f g = Flip . bimap g f . runFlip

instance Bifunctor p => Functor (Flip p a) where
  fmap f = Flip . first f . runFlip


newtype Zoom a = Zoom (a -> a)
  deriving (Semigroup, Monoid) via Endo a

newtype Foo a = Foo (Either a Int)
  deriving Functor via Flip Either Int

newtype Zoo a = Zoo (a -> Int)
  deriving Contravariant via Flip (->) Int

