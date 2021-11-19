-- # pragmas
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Defunc where

-- # imports
import Prelude hiding (fst)

fst :: (a, b) -> a
fst (a, _) = a

class Eval l t | l -> t where  -- ! 1
  eval :: l -> t

-- # EvalFst
instance Eval (Fst a b) a where
  eval (Fst (a, _)) = a

data Fst a b = Fst (a, b)

data MapList dfb a = MapList (a -> dfb) [a]  -- ! 1

-- # EvalMap
instance Eval dfb dft => Eval (MapList dfb a) [dft] where
  eval (MapList _ []) = []
  eval (MapList f (a : as)) =
    eval (f a) : eval (MapList f as)  -- ! 1


data ListToMaybe a = ListToMaybe [a]

-- # EvalListToMaybe
instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe [])      = Nothing
  eval (ListToMaybe (a : _)) = Just a

