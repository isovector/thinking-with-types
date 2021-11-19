-- # pragmas
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Roles where

-- # imports
import Data.Coerce (Coercible, coerce)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid (Sum (..), Product (..))

{-

newtype ZipList a = ZipList
  { getZipList :: [a]
  }

newtype Sum a = Sum
  { getSum :: a
  }

coerce :: Coercible a b => a -> b

insert :: Ord k => k -> v -> Map k v -> Map k v

-}

slowSum :: [Int] -> Int
slowSum = getSum . mconcat . fmap Sum

fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce

newtype Reverse a = Reverse
  { getReverse :: a
  } deriving (Eq, Show)

-- # OrdReverse
instance Ord a => Ord (Reverse a) where
  compare (Reverse a) (Reverse b) = compare b a

type family IntToBool a where
  IntToBool Int = Bool
  IntToBool a   = a



data BST v
  = Empty
  | Branch (BST v) v (BST v)

-- # role
type role BST nominal


