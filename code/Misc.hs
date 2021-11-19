-- # pragmas
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Misc where

import GHC.TypeLits

-- # showFunc
instance
    ( TypeError
        ( 'Text "Attempting to interpret a number as a function "
    ':$$: 'Text "in the type `"
    ':<>: 'ShowType (a -> b)
    ':<>: 'Text "'"
    ':$$: 'Text "Did you forget to specify the function you wanted?"
        )
    ) => Num (a -> b) where

{-

broken :: (a -> b) -> a -> b
broken f a = apply
  where
    apply :: b
    apply = f a

-}

working :: forall a b. (a -> b) -> a -> b
working f a = apply
  where
    apply :: b
    apply = f a

{-

-- # brokenWhy
broken :: (a -> b) -> a -> b
broken f a = apply
  where
    apply :: c
    apply = f a

-}


-- # Refl
data a :~: b where
  Refl :: a :~: a


data Proxy a = Proxy

{-

data Maybe a
  = Just a
  | Nothing

-}

