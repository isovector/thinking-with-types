-- # pragmas
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Printf where

-- # imports
import Data.Kind   (Type, Constraint)
import Data.Proxy  (Proxy (..))
import GHC.TypeLits

-- # typeList
type (:<<) :: k1 -> k2 -> Type
data a :<< b
infixr 5 :<<


type HasPrintf :: k -> Constraint
class HasPrintf a where
  type Printf a :: Type
  format :: String    -- ! 1
         -> Proxy a   -- ! 2
         -> Printf a  -- ! 3

-- # baseInstance
instance KnownSymbol text => HasPrintf (text :: Symbol) where
  type Printf text = String
  format s _ = s <> symbolVal (Proxy @text)

-- # textInstance
instance
     (HasPrintf a, KnownSymbol text)
  => HasPrintf ((text :: Symbol) :<< a)
  where
    type Printf (text :<< a) = Printf a
    format s _ = format (s <> symbolVal (Proxy @text))
                        (Proxy @a)

-- # paramInstance
instance (HasPrintf a, Show param)
    => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a
  format s _ param = format (s <> show param) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""

-- # stringInstance
instance {-# OVERLAPPING #-} HasPrintf a
    => HasPrintf (String :<< a) where
  type Printf (String :<< a) = String -> Printf a
  format s _ param = format (s <> param) (Proxy @a)

wrongPrintf :: a -> String -> String
wrongPrintf _ str = show str ++ " world!"

type Pad :: Nat -> k -> Type
data Pad n a

