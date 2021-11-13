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

data Meta
  = MetaSel
      (Maybe Symbol)  -- ! 1
      SourceUnpackedness
      SourceStrictness
      DecidedStrictness
  | ...

-}

type DemoteSymbolList :: [Symbol] -> Constraint
class DemoteSymbolList ts where
  demoteSymbolList :: Proxy ts -> [String]

-- # DemoteNil
instance DemoteSymbolList '[] where
  demoteSymbolList _ = []

-- # DemoteCons
instance (KnownSymbol t, DemoteSymbolList ts)
      => DemoteSymbolList (t ': ts)
         where
  demoteSymbolList _
    = symbolVal (Proxy @t)
    : demoteSymbolList (Proxy @ts)


type GEqOmit :: (k -> Type) -> Constraint
class GEqOmit f where
  geqomit :: [String] -> f x -> f x -> Bool

-- # GEqOmitC1
instance GEqOmit f => GEqOmit (C1 _1 f) where
  geqomit ignore (M1 a) (M1 b) =
    geqomit ignore a b

-- # GEqOmitD1
instance GEqOmit f => GEqOmit (D1 _1 f) where
  geqomit ignore (M1 a) (M1 b) =
    geqomit ignore a b

-- # GEqOmitS1Just
instance ( KnownSymbol name  -- ! 1
         , GEqOmit f
         )
      => GEqOmit (S1 ('MetaSel ('Just name) _1 _2 _3) f)
         where
  geqomit ignore (M1 a) (M1 b) =
    case elem (symbolVal $ Proxy @name) ignore of  -- ! 2
      True -> True
      False -> geqomit ignore a b

-- # GEqOmitS1Nothing
instance GEqOmit f
      => GEqOmit (S1 ('MetaSel 'Nothing _1 _2 _3) f)
         where
  geqomit ignore (M1 a) (M1 b) =
    geqomit ignore a b

-- # GEqOmitK1
instance Eq a
      => GEqOmit (K1 _1 a)
         where
  geqomit _ (K1 a) (K1 b) = a == b

-- # GEqOmitProduct
instance (GEqOmit f, GEqOmit g)
      => GEqOmit (f :*: g)
         where
  geqomit ignore (a1 :*: a2) (b1 :*: b2)
    = geqomit ignore a1 b1 && geqomit ignore a2 b2

-- # GEqOmitSum
instance (GEqOmit f, GEqOmit g)
      => GEqOmit (f :+: g)
         where
  geqomit ignore (L1 a) (L1 b)
    = geqomit ignore a b
  geqomit ignore (R1 a) (R1 b)
    = geqomit ignore a b
  geqomit _ _ _ = False

-- # GEqOmitU1
instance GEqOmit U1 where
  geqomit _ U1 U1 = True

-- # GEqOmitV1
instance GEqOmit V1 where
  geqomit _ _ _ = True


type Omit :: [Symbol] -> Type -> Type
newtype Omit oms a = Omit a

-- # EqOmit
instance ( DemoteSymbolList oms
         , Generic a
         , GEqOmit (Rep a)
         )
      => Eq (Omit oms a)
         where
  Omit a == Omit b =
    geqomit
      (demoteSymbolList $ Proxy @oms)
      (from a)
      (from b)


-- data Person = Person
--   { name     :: String
--   , age      :: Int
--   , metadata :: [String]
--   } deriving stock Generic
--     deriving Eq via (Omit '["age", "metadata"] Person)

