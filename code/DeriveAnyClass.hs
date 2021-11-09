-- # pragmas
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}

module DeriveAnyClass where

-- # imports
import GHC.Generics

{-

data Maybe a
  = Nothing
  | Just a

data Bool
  = False
  | True

-- # RepBool
Rep Bool
  = ...
      ( ... U1
    :+: ... U1
      )

-}

data Foo a b c
  = F0
  | F1 a
  | F2 b c
  deriving (Generic, MyEq)  -- ! 1

-- # EqFoo
instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
  (==) = genericEq

{-

-- # eqFoo
instance (Eq a, Eq b, Eq c)
      => Eq (Foo a b c) where
  F0       == F0       = True
  F1 a1    == F1 a2    = a1 == a2
  F2 b1 c1 == F2 b2 c2 = b1 == b2 && c1 == c2

-- # Foo1
data Foo a b c
  = F0
  | F1 a
  | F2 b c
  deriving (Generic)

-}

toCanonical :: Maybe a -> Either () a
toCanonical Nothing  = Left ()
toCanonical (Just a) = Right a

fromCanonical :: Either () a -> Maybe a
fromCanonical (Left ()) = Nothing
fromCanonical (Right a) = Just a

class GEq a where
  geq :: a x -> a x -> Bool

-- # geqU1
instance GEq U1 where
  geq U1 U1 = True

-- # geqV1
instance GEq V1 where
  geq _ _ = True

-- # geqK1
instance Eq a => GEq (K1 _1 a) where
  geq (K1 a) (K1 b) = a == b

-- # geqTimes
instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2

-- # geqPlus
instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L1 a1) (L1 a2) = geq a1 a2
  geq (R1 b1) (R1 b2) = geq b1 b2
  geq _ _             = False

-- # geqM1
instance GEq a => GEq (M1 _x _y a) where
  geq (M1 a1) (M1 a2) = geq a1 a2

class MyEq a where
  eq :: a -> a -> Bool
  default eq  -- ! 1
      :: (Generic a, GEq (Rep a))
      => a
      -> a
      -> Bool
  eq a b = geq (from a) (from b)

genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = geq (from a) (from b)


class GOrd a where
  gord :: a x -> a x -> Ordering

-- # gordU1
instance GOrd U1 where
  gord U1 U1 = EQ

-- # gordV1
instance GOrd V1 where
  gord _ _ = EQ

-- # gordK1
instance Ord a => GOrd (K1 _1 a) where
  gord (K1 a) (K1 b) = compare a b

-- # gordTimes
instance (GOrd a, GOrd b) => GOrd (a :*: b) where
  gord (a1 :*: b1) (a2 :*: b2) = gord a1 a2 <> gord b1 b2

-- # gordPlus
instance (GOrd a, GOrd b) => GOrd (a :+: b) where
  gord (L1 a1) (L1 a2) = gord a1 a2
  gord (R1 b1) (R1 b2) = gord b1 b2
  gord (L1 _)  (R1 _)  = LT
  gord (R1 _)  (L1 _)  = GT

-- # gordM1
instance GOrd a => GOrd (M1 _x _y a) where
  gord (M1 a1) (M1 a2) = gord a1 a2

genericOrd :: (Generic a, GOrd (Rep a)) => a -> a -> Ordering
genericOrd a b = gord (from a) (from b)


class GExNihilo a where
  gexNihilo :: Maybe (a x)

-- # gexNihiloU1
instance GExNihilo U1 where
  gexNihilo = Just U1

-- # gexNihiloV1
instance GExNihilo V1 where
  gexNihilo = Nothing

-- # gexNihiloPlus
instance GExNihilo (a :+: b) where
  gexNihilo = Nothing

-- # gexNihiloTimes
instance GExNihilo (a :*: b) where
  gexNihilo = Nothing

-- # gexNihiloK1
instance GExNihilo (K1 _1 a) where
  gexNihilo = Nothing

-- # gexNihiloM1
instance GExNihilo a => GExNihilo (M1 _x _y a) where
  gexNihilo = fmap M1 gexNihilo

exNihilo :: (Generic a, GExNihilo (Rep a)) => Maybe a
exNihilo = fmap to gexNihilo



{-

class Generic a where
  type Rep a :: Type -> Type  -- ! 1
  from :: a -> Rep a x  -- ! 2
  to   :: Rep a x -> a  -- ! 3

-}

