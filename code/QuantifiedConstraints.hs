{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module QuantifiedConstraints where

import Data.Coerce
import Data.Maybe
import Data.Functor.Identity
import Control.Applicative

class (forall m. Monad m => Monad (t m)) => MonadTrans t where
  lift :: Monad m => m a -> t m a

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  } deriving Functor

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  liftA2 f (MaybeT a) (MaybeT b) = coerce $ liftA2 (liftA2 f) a b

instance Monad m => Monad (MaybeT m) where
  MaybeT m >>= f = MaybeT $ maybe (pure Nothing) (runMaybeT . f) =<< m



instance MonadTrans MaybeT where
  lift = MaybeT . fmap pure

getHead :: MonadTrans t => [a] -> t Maybe a
getHead as = do
  x <- lift $ listToMaybe as
  pure x


type family HKD f a where
  HKD Identity a = a
  HKD f a = f a


data Foo f x = Foo
  { zoo :: HKD f Int
  , zum :: HKD f Bool
  , zap :: HKD f x
  }

class    (Eq (HKD f a)) => EqQ f a
instance (Eq (HKD f a)) => EqQ f a


deriving instance (Eq (HKD f Int), Eq (HKD f Bool), Eq (HKD f x)) => Eq (Foo f x)


-- instance (Eq x, forall a. Eq a => EqQ f a) => Eq (Foo f x) where
--   Foo a b c == Foo x y z =
--     with @(EqQ f Int)  $
--     with @(EqQ f Bool) $
--     with @(EqQ f x)    $
--       a == x && b == y && c == z

yo :: Eq a => Foo Identity a -> Foo Identity a -> Bool
yo = (==)

with :: (c => t) -> (c => t)
with x = x



