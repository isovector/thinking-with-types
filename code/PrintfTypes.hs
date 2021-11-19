{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module PrintfTypes where

import Data.Kind   (Type, Constraint)
import GHC.TypeLits

data (a :: k1) :<< (b :: k2)
infixr 5 :<<

type HasPrintf :: k -> Constraint
class HasPrintf a where  -- ! 1
  type Printf a :: Type  -- ! 2

-- # baseInstance
instance HasPrintf (text :: Symbol) where
  type Printf text = String

-- # textInstance
instance HasPrintf a
    => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a) = Printf a

-- # paramInstance
instance HasPrintf a
    => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a



type family AlwaysUnit a where
  AlwaysUnit a = ()

class TypeName a where
  typeName :: AlwaysUnit a -> String

-- # TypeNameString
instance TypeName String where
  typeName _ = "String"

-- # TypeNameBool
instance TypeName Bool where
  typeName _ = "Bool"

{-

name :: String
name = typeName ()

-}

