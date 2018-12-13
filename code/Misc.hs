-- # pragmas
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Misc where

import GHC.TypeLits

-- # showFunc
instance
    ( TypeError
        ( Text "Attempting to show a function of type `"
     :<>: ShowType (a -> b)
     :<>: Text "'"
     :$$: Text "Did you forget to apply an argument?"
        )
    ) => Show (a -> b) where
  show = undefined

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

