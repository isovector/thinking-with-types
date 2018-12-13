module PosNeg where

newtype T1 a = T1 (Int -> a)

-- # FunctorT1
instance Functor T1 where
  fmap f (T1 a) = T1 $ fmap f a

newtype T2 a = T2 (a -> Int)

newtype T3 a = T3 (a -> a)

newtype T4 a = T4 ((Int -> a) -> Int)

newtype T5 a = T5 ((a -> Int) -> Int)

-- # FunctorT5
instance Functor T5 where
  fmap f (T5 aii) = T5 $ \bi -> aii $ bi . f

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

class Invariant f where
  invmap :: (a -> b) -> (b -> a) -> f a -> f b

