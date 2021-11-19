-- # pragmas
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module FCTF where

-- # imports
import Data.Kind (Constraint, Type)

data Map :: (a -> Exp b) -> f a -> Exp (f b)

-- # Map4Lists
type instance Eval (Map f '[]) = '[]
type instance Eval (Map f (a ': as)) = Eval (f a) ': Eval (Map f as)

-- # Map4Maybe
type instance Eval (Map f 'Nothing)  = 'Nothing
type instance Eval (Map f ('Just a)) = 'Just (Eval (f a))

-- # Map4Either
type instance Eval (Map f ('Left x))  = 'Left x
type instance Eval (Map f ('Right a)) = 'Right (Eval (f a))

-- # MapTuple
type instance Eval (Map f '(a, b))  = '(a, Eval (f b))


type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b

-- # EvalSnd
type instance Eval (Snd '(a, b)) = b

data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval (FromMaybe _1 ('Just a)) = a
type instance Eval (FromMaybe a 'Nothing)   = a

data MapList :: (a -> Exp b) -> [a] -> Exp [b]

-- # EvalNil
type instance Eval (MapList f '[]) = '[]

-- # EvalCons
type instance Eval (MapList f (a ': as))
  = Eval (f a) ': Eval (MapList f as)


data Fmap :: (a -> b) -> Exp a -> Exp b
type instance Eval (Fmap f fa) = f (Eval fa)




data Pure :: a -> Exp a
type instance Eval (Pure x) = x

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval (Pure1 f x) = f x

data Pure2 :: (a -> b -> c) -> a -> b -> Exp c
type instance Eval (Pure2 f x y) = f x y

data Pure3 :: (a -> b -> c -> d) -> a -> b -> c -> Exp d
type instance Eval (Pure3 f x y z) = f x y z

data (++) :: [a] -> [a] -> Exp [a]
type instance Eval ((++) '[] ys) = ys
type instance Eval ((++) (x ': xs) ys) = x ': Eval ((++) xs ys)

-- # bind
data (=<<)
    :: (a -> Exp b)
    -> Exp a
    -> Exp b
type instance Eval (k =<< e) =
  Eval (k (Eval e))
infixr 0 =<<

-- # kleisli
data (<=<)
    :: (b -> Exp c)
    -> (a -> Exp b)
    -> a -> Exp c
type instance Eval ((f <=< g) x) =
  Eval (f (Eval (g x)))
infixr 1 <=<

data Join :: Exp (Exp a) -> Exp a
type instance Eval (Join e) = Eval (Eval e)

data TyEq :: a -> b -> Exp Bool

-- # EvalTyEq
type instance Eval (TyEq a b) = TyEqImpl a b

type family TyEqImpl (a :: k) (b :: k) :: Bool where
  TyEqImpl a a = 'True
  TyEqImpl a b = 'False

data Collapse :: [Constraint] -> Exp Constraint
type instance Eval (Collapse '[]) =
  (() :: Constraint)
type instance Eval (Collapse (a ': as)) =
  (a, Eval (Collapse as))

type All (c :: k -> Constraint) (ts :: [k]) =
  Collapse =<< MapList (Pure1 c) ts

infixr 0 $
data ($) :: (a -> Exp b) -> a -> Exp b
type instance Eval (($) f a) = Eval (f a)

data Flip :: (a -> b -> Exp c) -> b -> a -> Exp c
type instance Eval (Flip f y x) = Eval (f x y)

data Mempty :: k -> Exp k

-- # mempties
type instance Eval
  (Mempty '()) = '()
type instance Eval
  (Mempty (c :: Constraint))
    = (() :: Constraint)
type instance Eval
  (Mempty (l :: [k])) = '[]
-- etc

data Mappend :: a -> a -> Exp a
type instance Eval
  (Mappend '() '()) = '()
type instance Eval
  (Mappend (a :: Constraint)
           (b :: Constraint)) = (a, b)
type instance Eval
  (Mappend (a :: [k])
           (b :: [k])) = Eval (a ++ b)
-- etc

data ListToMaybe :: [a] -> Exp (Maybe a)
type instance Eval (ListToMaybe '[])       = 'Nothing
type instance Eval (ListToMaybe (a ': _1)) = 'Just a

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr _1 b '[]) = b
type instance Eval (Foldr f b (a ': as)) =
  Eval (f a (Eval (Foldr f b as)))

