-- # pragmas
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

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

