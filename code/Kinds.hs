-- # pragmas
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Kinds where

-- # imports
import Prelude (IO)

-- # typelits
import GHC.TypeLits

data Bool
  = True
  | False

or :: Bool -> Bool -> Bool
or True  _ = True
or False y = y

-- # TFOr
type Or :: Bool -> Bool -> Bool
type family Or x y where
  Or 'True  y = 'True
  Or 'False y = y

type And :: Bool -> Bool -> Bool
type family And x y where
  And 'True  y = y
  And 'False y = 'False

type Not :: Bool -> Bool
type family Not x where
  Not 'True  = 'False
  Not 'False = 'True

map :: (a -> b) -> [a] -> [b]
map _ []       = []
map f (a : as) = f a : map f as

-- # TFMap
type Map :: (a -> b) -> [a] -> [b]
type family Map f xs where
  Map f '[]       = '[]
  Map f (x ': xs) = f x ': Map f xs

type Foo :: Bool -> Bool -> Bool
type family Foo x y

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
