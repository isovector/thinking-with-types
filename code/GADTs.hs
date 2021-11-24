-- # pragmas
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -Wall #-}

module GADTs where

-- # imports
import Data.Kind (Constraint, Type)

data Expr a where  -- ! 1
  LitInt  :: Int -> Expr Int  -- ! 2
  LitBool :: Bool -> Expr Bool
  Add     :: Expr Int -> Expr Int -> Expr Int
  Not     :: Expr Bool -> Expr Bool
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a  -- ! 3

data Expr_ a
  = (a ~ Int)  => LitInt_  Int
  | (a ~ Bool) => LitBool_ Bool
  | (a ~ Int)  => Add_ (Expr_ Int) (Expr_ Int)
  | (a ~ Bool) => Not_ (Expr_ Bool)
  | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)

evalExpr :: Expr a -> a
evalExpr (LitInt i)  = i  -- ! 1
evalExpr (LitBool b) = b  -- ! 2
evalExpr (Add x y)   = evalExpr x + evalExpr y
evalExpr (Not x)     = not $ evalExpr x
evalExpr (If b x y)  =
  if evalExpr b
     then evalExpr x
     else evalExpr y


evalExpr_ :: Expr_ a -> a
evalExpr_ (LitInt_ i)  = i
evalExpr_ (LitBool_ b) = b
evalExpr_ (Add_ x y)   = evalExpr_ x + evalExpr_ y
evalExpr_ (Not_ x)     = not $ evalExpr_ x
evalExpr_ (If_ b x y)  =
  if evalExpr_ b
     then evalExpr_ x
     else evalExpr_ y


data HList (ts :: [Type]) where -- ! 1
  HNil :: HList '[]  -- ! 2
  (:#) :: t -> HList ts -> HList (t ': ts)  -- ! 3
infixr 5 :#

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

showBool :: HList '[a, Bool, b] -> String
showBool (_ :# b :# _ :# HNil) = show b

hLength :: HList ts -> Int
hLength HNil      = 0
hLength (_ :# ts) = 1 + hLength ts

{-

-- # eqHNil
instance Eq (HList '[]) where
  HNil == HNil = True

-- # eqHCons
instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs

-- # ordHNil
instance Ord (HList '[]) where
  compare HNil HNil = EQ

-- # ordHCons
instance (Ord t, Ord (HList ts))
    => Ord (HList (t ': ts)) where
  compare (a :# as) (b :# bs) =
    compare a b <> compare as bs

-- # showHNil
instance Show (HList '[]) where
  show HNil = "HNil"

-- # showHCons
instance (Show t, Show (HList ts))
    => Show (HList (t ': ts)) where
  show (a :# as) = show a <> " :# " show as

-}

-- # eqHList
instance All Eq ts => Eq (HList ts) where
  HNil      == HNil      = True
  (a :# as) == (b :# bs) = a == b && as == bs

-- # ordHList
instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil = EQ
  compare (a :# as) (b :# bs) =
    compare a b <> compare as bs

-- # showHList
instance (All Show ts) => Show (HList ts) where
  show HNil = "HNil"
  show (a :# as) = show a <> " :# " <> show as


type family All (c :: Type -> Constraint)
                (ts :: [Type]) :: Constraint where
  All c '[]       = ()  -- ! 1
  All c (t ': ts) = (c t, All c ts)  -- ! 2

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[]       = ()  -- ! 1
  AllEq (t ': ts) = (Eq t, AllEq ts)  -- ! 2

-- foldHList
--     :: forall c ts m
--      . (All c ts, Monoid m)
--     => (forall t. c t => t -> m)
--     -> HList ts
--     -> m
-- foldHList _ HNil = mempty
-- foldHList f (a :# as) = f a <> foldHList @c f as


