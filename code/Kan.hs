{-# OPTIONS_GHC -Wno-unused-imports #-}

-- # pragmas
module Kan where

-- # imports
import Data.Functor.Yoneda
import Data.Functor.Day.Curried
import Control.Monad.Codensity

{-

newtype Yoneda f a = Yoneda
  { runYoneda :: forall b. (a -> b) -> f b
  }

-- # FunctorYoneda
instance Functor (Yoneda f) where
  fmap f (Yoneda y) = Yoneda (\k -> y (k . f))

-}

