{-# LANGUAGE AllowAmbiguousTypes #-}

module TypeApps where

import Data.Typeable


typeName :: forall a. Typeable a => String -- ! 1
typeName = show . typeRep $ Proxy @a  -- ! 2

