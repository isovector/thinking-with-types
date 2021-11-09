-- # pragmas
{-# LANGUAGE UndecidableInstances #-}

module RankN where

-- # imports
import Control.Applicative (Alternative (..))
import Data.Foldable (asum)
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Typeable (Typeable, cast, typeRep)

import Control.Monad.Trans.Class (MonadTrans (..))

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

{-

-- # forall1
forall a. a -> a

-- # forall2
forall a. (a -> a)

-- # forall3
forall r. ((forall a. (a -> r)) -> r)

id :: forall a. a -> a
id a = a

-- # brokenApply
applyToFive :: (a -> a) -> Int
applyToFive f = f 5

-- # explicitBrokenApply
applyToFive :: forall a. (a -> a) -> Int
applyToFive f = f 5

-}

cont :: a -> (forall r. (a -> r) -> r)
cont a = \callback -> callback a


isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty a = a == mempty

runCont :: (forall r. (a -> r) -> r) -> a
runCont f =
  let callback = id
   in f callback


newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

-- # contFunctor
instance Functor Cont where
  fmap f (Cont c) = Cont $ \c' ->
    c (c' . f)

-- # contApplicative
instance Applicative Cont where
  pure a = Cont $ \c -> c a
  Cont f <*> Cont a = Cont $ \br ->
    f $ \ab ->
      a $ br . ab

-- # contMonad
instance Monad Cont where
  return = pure
  Cont m >>= f = Cont $ \c ->
    m $ \a ->
      unCont (f a) c

newtype ContT m a = ContT
  { unContT :: forall r. (a -> m r) -> m r
  }

instance Functor (ContT m) where
  fmap f (ContT c) = ContT $ \c' -> c (c' . f)

instance Applicative (ContT m) where
  pure a = ContT $ \c -> c a
  ContT f <*> ContT a = ContT $ \br -> f $ \ab -> a $ br . ab

instance Monad (ContT m) where
  return = pure
  ContT m >>= f = ContT $ \c ->
    m $ \a ->
      unContT (f a) c

instance MonadTrans ContT where
  lift m = ContT $ (m >>=)

releaseString :: String
releaseString =
  withVersionNumber $ \version ->
    withTimestamp $ \date ->
      withOS $ \os ->
        os ++ "-" ++ show version ++ "-" ++ show date


withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withTimestamp :: (Int -> r) -> r
withTimestamp f = f 1532083362

withOS :: (String -> r) -> r
withOS f = f "linux"


releaseStringCont :: String
releaseStringCont = runCont $ unCont $ do
  version <- Cont withVersionNumber
  date    <- Cont withTimestamp
  os      <- Cont withOS
  pure $ os ++ "-" ++ show version ++ "-" ++ show date


data Any = forall a. Any a

elimAny :: (forall a. a -> r) -> Any -> r
elimAny f (Any a) = f a

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas
    :: (forall a. c a => a -> r)
    -> Has c
    -> r
elimHas f (Has a) = f a

data HasShow where
  HasShow :: Show t => t -> HasShow

elimHasShow
    :: (forall a. Show a => a -> r)
    -> HasShow
    -> r
elimHasShow f (HasShow a) = f a

-- # hasShowShow
instance Show HasShow where
  show (HasShow s) = "HasShow " ++ show s

{-

-- # showElimHasShow
instance Show HasShow where
  show = elimHasShow show

-}


data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic
    :: (forall a. Typeable a => a -> r)
    -> Dynamic
    -> r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2
    :: forall a b r.
       ( Typeable a
       , Typeable b
       , Typeable r
       )
    => Dynamic
    -> Dynamic
    -> (a -> b -> r)
    -> Maybe Dynamic
liftD2 d1 d2 f =
    fmap Dynamic . f
      <$> fromDynamic @a d1
      <*> fromDynamic @b d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
  fromMaybe (error "bad types for pyPlus") $ asum
    [ liftD2 @String @String a b (++)
    , liftD2 @Int    @Int    a b (+)
    , liftD2 @String @Int    a b $ \strA intB ->
        strA ++ show intB
    , liftD2 @Int    @String a b $ \intA strB ->
        show intA ++ strB
    ]


typeOf :: Dynamic -> String
typeOf = elimDynamic $ \(a :: t) ->
  show . typeRep $ Proxy @t

type MonoidAndEq a = (Monoid a, Eq a)

-- # MonoidEq
class    (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a


{-

-- # GADTAny
data Any where
  Any :: a -> Any

-- # typeHasShow
type HasShow = Has Show
type Dynamic = Has Typeable

-}

