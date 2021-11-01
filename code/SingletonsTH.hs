-- # pragmas
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module SingletonsTH where

-- # imports
import Data.Singletons.Prelude
import Data.Singletons.TH

-- # TimeOfDay
singletons [d|
  data TimeOfDay
    = Morning
    | Afternoon
    | Evening
    | Night
    deriving (Eq, Ord, Show)
  |]

