-- # pragmas
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}

module DeriveAnyClass where

-- # imports
import GHC.Generics

{-

data Maybe a
  = Nothing
  | Just a

data Bool
  = False
  | True

-- # RepBool
Rep Bool
  = ...
      ( ... U1
    :+: ... U1
      )

-}

-- # eqFoo
instance (Eq a, Eq b, Eq c)
      => Eq (Foo a b c)
         where
  F0       == F0       = True
  F1 a1    == F1 a2    = a1 == a2
  F2 b1 c1 == F2 b2 c2 = b1 == b2 && c1 == c2
  _        == _        = False

-- # Foo1
data Foo a b c
  = F0
  | F1 a
  | F2 b c
  deriving (Generic)

toCanonical :: Maybe a -> Either () a
toCanonical Nothing  = Left ()
toCanonical (Just a) = Right a

fromCanonical :: Either () a -> Maybe a
fromCanonical (Left ()) = Nothing
fromCanonical (Right a) = Just a


{-

class Generic a where
  type Rep a :: Type -> Type  -- ! 1
  from :: a -> Rep a x  -- ! 2
  to   :: Rep a x -> a  -- ! 3

-}

