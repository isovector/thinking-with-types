-- # pragmas
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module SingletonsTH where

-- # imports
import Data.Ord.Singletons
import Data.Singletons.TH
import Prelude.Singletons

-- # TimeOfDay
singletons [d|
  data TimeOfDay
    = Morning
    | Afternoon
    | Evening
    | Night
    deriving (Eq, Ord, Show)
  |]

