module Generic.Monoid where

import Data.Kind
import GHC.Generics

type GSemigroup :: (k -> Type) -> Constraint
class GSemigroup a where
  gappend :: a x -> a x -> a x

-- ! GSemigroupM1
instance GSemigroup f => GSemigroup (M1 _1 _2 f) where
  gappend (M1 a) (M1 b) = M1 $ gappend a b

-- ! GSemigroupProduct
instance (GSemigroup f, GSemigroup g) => GSemigroup (f :*: g) where
  gappend (a1 :*: b1) (a2 :*: b2) = gappend a1 a2 :*: gappend b1 b2

-- ! GSemigroupK1
instance Semigroup a => GSemigroup (K1 _1 a) where
  gappend (K1 a) (K1 b) = K1 $ a <> b

-- ! GSemigroupU1
instance GSemigroup U1 where
  gappend U1 U1 = U1

-- ! GSemigroupV1
instance GSemigroup V1 where
  gappend v _ = case v of {}


type GMonoid :: (k -> Type) -> Constraint
class GMonoid a where
  gempty :: a x

-- ! GMonoidM1
instance GMonoid f => GMonoid (M1 _1 _2 f) where
  gempty = M1 gempty

-- ! GMonoidProduct
instance (GMonoid f, GMonoid g) => GMonoid (f :*: g) where
  gempty = gempty :*: gempty

-- ! GMonoidK1
instance Monoid a => GMonoid (K1 _1 a) where
  gempty = K1 mempty

-- ! GMonoidU1
instance GMonoid U1 where
  gempty = U1

-- NO V1 INSTANCE ON PURPOSE

