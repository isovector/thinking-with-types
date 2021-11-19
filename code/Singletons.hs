-- # pragmas
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Singletons where

-- # imports
import Data.Typeable
import Data.Void
import Unsafe.Coerce (unsafeCoerce)

data family Sing (a :: k)

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

class SingKind k where
  type Demote k = r | r -> k -- ! 1
  toSing :: Demote k -> SomeSing k
  fromSing :: Sing (a :: k) -> Demote k  -- ! 2

class SingI (a :: k) where
  sing :: Sing a


withSomeSing
    :: SomeSing k
    -> (forall (a :: k). Sing a -> r)
    -> r
withSomeSing (SomeSing s) f = f s


-- # SingBool
data instance Sing (a :: Bool) where
  STrue  :: Sing 'True
  SFalse :: Sing 'False

-- # SingITrue
instance SingI 'True where
  sing = STrue

-- # SingIFalse
instance SingI 'False where
  sing = SFalse


-- # SingKindBool
instance SingKind Bool where
  type Demote Bool = Bool
  toSing True  = SomeSing STrue
  toSing False = SomeSing SFalse
  fromSing STrue  = True
  fromSing SFalse = False


-- # SingMaybe
data instance Sing (a :: Maybe k) where
  SJust    :: Sing (a :: k) -> Sing ('Just a)
  SNothing :: Sing 'Nothing

-- # SingINothing
instance SingI 'Nothing where
  sing = SNothing

-- # SingIJust
instance SingI a => SingI ('Just a) where
  sing = SJust sing

-- # SingKindMaybe
instance (k ~ Demote k, SingKind k)
      => SingKind (Maybe k) where
  type Demote (Maybe k) = Maybe k
  toSing (Just a) =
    withSomeSing (toSing a) $ SomeSing . SJust
  toSing Nothing = SomeSing SNothing
  fromSing (SJust a) = Just $ fromSing a
  fromSing SNothing = Nothing


-- # SingList
data instance Sing (a :: [k]) where
  SNil  :: Sing '[]
  SCons :: Sing (h :: k)
        -> Sing (t :: [k])
        -> Sing (h ': t)

-- # SingKindList
instance (k ~ Demote k, SingKind k)
      => SingKind [k] where
  type Demote [k] = [k]
  toSing []  = SomeSing SNil
  toSing (h : t) =
    withSomeSing (toSing h) $ \sh ->
      withSomeSing (toSing t) $ \st ->
        SomeSing $ SCons sh st
  fromSing SNil = []
  fromSing (SCons sh st) =
    fromSing sh : fromSing st

-- # SingINil
instance SingI '[] where
  sing = SNil

-- # SingICons
instance (SingI h, SingI t) => SingI (h ': t) where
  sing = SCons sing sing

data Decision a
  = Proved a
  | Disproved (a -> Void)  -- ! 1

class SDecide k where
  (%~) :: Sing (a :: k)
       -> Sing (b :: k)
       -> Decision (a :~: b)

-- # FreeSDecide
instance (Eq (Demote k), SingKind k)
      => SDecide k where
  a %~ b =
    if fromSing a == fromSing b
       then Proved $ unsafeCoerce Refl
       else Disproved $ const undefined


-- # SDecideBool
instance SDecide Bool where
  STrue  %~ STrue  = Proved Refl
  SFalse %~ SFalse = Proved Refl
  _ %~ _ = Disproved $ const undefined


-- # SDecideMaybe
instance SDecide a => SDecide (Maybe a) where
  SJust a %~ SJust b =
    case a %~ b of
      Proved Refl -> Proved Refl
      Disproved _ -> Disproved $ const undefined
  SNothing %~ SNothing = Proved Refl
  _ %~ _ = Disproved $ const undefined


