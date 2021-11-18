{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.Omit where

import Data.Kind
import GHC.TypeLits
import Data.Proxy
import GHC.Generics

data Weird r = Weird
  { name   :: String
  , value  :: Int
  , result :: Int -> r
  }
  deriving Generic
  deriving Eq via (Omit '["result"] (Weird r))

{-

-- # BadWeird
data Weird r = Weird
  { name   :: String
  , value  :: Int
  , result :: Int -> r
  }
  deriving Eq

data Meta
  = MetaSel
      (Maybe Symbol)  -- ! 1
      SourceUnpackedness
      SourceStrictness
      DecidedStrictness
  | ...

-}


type GEqOmit :: [Symbol] -> (k -> Type) -> Constraint
class GEqOmit o f where
  geqomit :: Proxy o -> f x -> f x -> Bool

-- # GEqOmitC1
instance
    GEqOmit o f
 => GEqOmit o (C1 _1 f)
    where
  geqomit o (M1 a) (M1 b) =
    geqomit o a b

-- # GEqOmitD1
instance
    GEqOmit o f
 => GEqOmit o (D1 _1 f)
    where
  geqomit o (M1 a) (M1 b) =
    geqomit o a b

-- # GEqOmitS1NotOmitted
instance
    GEqOmit '[] f
 => GEqOmit
      '[]
      (S1 ('MetaSel ('Just name) _1 _2 _3) f)
    where
  geqomit o (M1 a) (M1 b) = geqomit o a b

-- # GEqOmitS1Omitted
instance
    GEqOmit
      (name ': o)
      (S1 ('MetaSel ('Just name) _1 _2 _3) f)
    where
  geqomit _ _ _ = True

-- # GEqOmitS1Induction
instance
    {-# OVERLAPPABLE #-}  -- ! 3
    GEqOmit
      o  -- ! 4
      (S1 ('MetaSel ('Just name) _1 _2 _3) f)
 => GEqOmit
      (other_name ': o)  -- ! 1
      (S1 ('MetaSel ('Just name) _1 _2 _3) f) -- ! 2
    where
  geqomit _ a b =
    geqomit (Proxy @o)  -- ! 5
      a b

-- # GEqOmitS1Nothing
instance
    GEqOmit o f
 => GEqOmit o (S1 ('MetaSel 'Nothing _1 _2 _3) f)
    where
  geqomit o (M1 a) (M1 b) =
    geqomit o a b

-- # GEqOmitK1
instance
    Eq a
 => GEqOmit o (K1 _1 a)
    where
  geqomit _ (K1 a) (K1 b) = a == b

-- # GEqOmitProduct
instance
    (GEqOmit o f, GEqOmit o g)
 => GEqOmit o (f :*: g)
    where
  geqomit o (a1 :*: a2) (b1 :*: b2)
    = geqomit o a1 b1 && geqomit o a2 b2

-- # GEqOmitSum
instance
    (GEqOmit o f, GEqOmit o g)
 => GEqOmit o (f :+: g)
    where
  geqomit o (L1 a) (L1 b)
    = geqomit o a b
  geqomit o (R1 a) (R1 b)
    = geqomit o a b
  geqomit _ _ _ = False

-- # GEqOmitU1
instance GEqOmit o U1 where
  geqomit _ U1 U1 = True

-- # GEqOmitV1
instance GEqOmit o V1 where
  geqomit _ _ _ = True


type Omit :: [Symbol] -> Type -> Type
newtype Omit o a = Omit a

-- # EqOmit
instance
    ( Generic a
    , GEqOmit o (Rep a)
    )
 => Eq (Omit o a)
    where
  Omit a == Omit b =
    geqomit
      (Proxy @o)
      (from a)
      (from b)


-- data Person = Person
--   { name     :: String
--   , age      :: Int
--   , metadata :: [String]
--   } deriving stock Generic
--     deriving Eq via (Omit '["age", "metadata"] Person)

