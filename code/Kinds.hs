-- # pragmas
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Kinds where

-- # imports
import Prelude (IO)
import Data.Proxy (Proxy (..))

-- # typelits
import GHC.TypeLits

data Bool
  = True
  | False

or :: Bool -> Bool -> Bool
or True  _ = True
or False y = y

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True  y = 'True
  Or 'False y = y

type family And (x :: Bool) (y :: Bool) :: Bool where
  And 'True  y = y
  And 'False y = 'False

type family Not (x :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True

map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (a : as) = f a : map f as

type family Map (x :: a -> b) (i :: [a]) :: [b] where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

type family Foo (x :: Bool) (y :: Bool) :: Bool

type family Bar x y :: Bool -> Bool -> Bool

data Unit = Unit

{-

data User = User
  { userAdminToken :: Maybe (Proxy 'Admin)
  , ...
  }

doSensitiveThings :: Proxy 'Admin -> IO ()
doSensitiveThings = ...

-}

data UserType
  = User
  | Admin

{-

-- # kind
kind Bool
  = 'True
  | 'False

-- # list
data [a]
  = []
  | a : [a]

-}
