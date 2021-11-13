{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module Generic.Monoid where

import Data.Kind
import GHC.Generics

import Data.Monoid
import Data.Coerce (coerce)

type GSemigroup :: (k -> Type) -> Constraint
class GSemigroup f where
  gappend :: f x -> f x -> f x

data BigProduct a = BigProduct
  { bp_total  :: Sum Int
  , bp_valid  :: All
  , bp_recent :: Last a
  , bp_names  :: [String]
  }
  deriving (Show, Generic)

genericMappend
    :: ( Generic a  -- ! 1
       , GSemigroup (Rep a)  -- ! 2
       )
    => a -> a -> a
genericMappend a b = to $ gappend (from a) (from b)  -- ! 3

genericMempty :: (Generic a , GMonoid (Rep a)) => a
genericMempty = to gmempty

-- # BigProductSemigroup
instance Semigroup (BigProduct a) where
  BigProduct a1 b1 c1 d1
      <> BigProduct a2 b2 c2 d2 =
    BigProduct
      (a1 <> a2)
      (b1 <> b2)
      (c1 <> c2)
      (d1 <> d2)

{-

-- # BigProductGSemigroup
instance Semigroup (BigProduct a) where
  (<>) = genericMappend

-}

-- # GSemigroupM1
instance GSemigroup f
      => GSemigroup (M1 _1 _2 f)
         where
  gappend (M1 a) (M1 b) = M1 $ gappend a b

-- # GSemigroupProduct
instance (GSemigroup f, GSemigroup g)
      => GSemigroup (f :*: g)
         where
  gappend (a1 :*: b1) (a2 :*: b2) =
    gappend a1 a2 :*: gappend b1 b2

-- # GSemigroupK1
instance Semigroup a
      => GSemigroup (K1 _1 a)
         where
  gappend (K1 a) (K1 b) = K1 $ a <> b

-- # GSemigroupU1
instance GSemigroup U1 where
  gappend U1 U1 = U1

-- # GSemigroupV1
instance GSemigroup V1 where
  gappend v _ = case v of {}  -- ! 1


type GMonoid :: (k -> Type) -> Constraint
class GMonoid f where
  gmempty :: f x

-- # GMonoidM1
instance GMonoid f
      => GMonoid (M1 _1 _2 f)
         where
  gmempty = M1 gmempty

-- # GMonoidProduct
instance (GMonoid f, GMonoid g)
      => GMonoid (f :*: g)
         where
  gmempty = gmempty :*: gmempty

-- # GMonoidK1
instance Monoid a
      => GMonoid (K1 _1 a)
         where
  gmempty = K1 mempty

-- # GMonoidU1
instance GMonoid U1 where
  gmempty = U1


newtype Generically a = Generically a

newtype Generically2 a = Generically2 a

-- # GenericallySemigroup
instance (Generic a, GSemigroup (Rep a))
      => Semigroup (Generically a)
         where
  (<>) = coerce $ genericMappend @a

-- # GenericallySemigroup2
instance (Generic a, GSemigroup (Rep a))
      => Semigroup (Generically2 a)
         where
  Generically2 a <> Generically2 b =
    Generically2 $ genericMappend a b

-- # GenericallyMonoid
instance ( Generic a
         , GMonoid (Rep a)
         , Semigroup (Generically a)
         )
      => Monoid (Generically a)
         where
  mempty = coerce $ genericMempty @a

data Foo = Foo
  { f_total  :: Sum Int
  , f_valid  :: All
  }
  deriving (Show, Generic)
  deriving Semigroup
    via Generically Foo

{-

-- # Foo2
data Foo = Foo
  { f_total  :: Sum Int
  , f_valid  :: All
  }
  deriving (Show, Generic)
  deriving (Semigroup, Monoid)
    via Generically Foo

-}

