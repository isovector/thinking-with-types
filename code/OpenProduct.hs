-- # pragmas
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE TypeInType  #-}

module OpenProduct where

-- # imports
import           Data.Kind (Constraint, Type)
import           Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import           GHC.OverloadedLabels (IsLabel (..))
import           GHC.TypeLits
import           Unsafe.Coerce (unsafeCoerce)
import           Fcf

import Data.Constraint


data Any (f :: k -> Type) where
  Any :: f t -> Any f


data OpenProduct (f  :: k -> Type)
                 (ts :: [(Symbol, k)]) where  -- ! 1
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty


data Key (key :: Symbol) = Key


-- # keyIsLabel
instance (key ~ key') -- ! 1
      => IsLabel key (Key key') where
  fromLabel = Key

type UniqueKey (key :: k) (ts :: [(k, t)])
  = Null =<< Filter (TyEq key <=< Fst) ts

badInsert
    :: Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f ('(key, t) ': ts)
badInsert _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v

oldInsert
    :: Eval (UniqueKey key ts) ~ 'True
    => Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f ('(key, t) ': ts)
oldInsert _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v

type family RequireUniqueKey
      (result :: Bool)  -- ! 1
      (key :: Symbol)
      (t :: k)
      (ts :: [(Symbol, k)]) :: Constraint where
  RequireUniqueKey 'True  key t ts = ()  -- ! 2
  RequireUniqueKey 'False key t ts =
    TypeError
         ( 'Text "Attempting to add a field named `"
     ':<>: 'Text key
     ':<>: 'Text "' with type "
     ':<>: 'ShowType t
     ':<>: 'Text " to an OpenProduct."
     ':$$: 'Text "But the OpenProduct already has a field `"
     ':<>: 'Text key
     ':<>: 'Text "' with type "
     ':<>: 'ShowType (LookupType key ts)
     ':$$: 'Text "Consider using `update' "  -- ! 3
     ':<>: 'Text "instead of `insert'."
         )

insert
    :: RequireUniqueKey (Eval (UniqueKey key ts)) key t ts
    => Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v

-- upsert
--     :: Key key
--     -> f t
--     -> OpenProduct f ts
--     -> OpenProduct f (Eval (UpsertElem key t ts))
-- upsert = undefined

data Placeholder1Of3
    :: (a -> b -> c -> Exp r)
    -> b
    -> c
    -> a
    -> Exp r

-- # EvalPlaceholder
type instance Eval (Placeholder1Of3 f b c a) =
  Eval (f a b c)

type UpsertLoc (key :: Symbol)
               (ts :: [(Symbol, k)]) =
  Eval (FindIndex (TyEq key <=< Fst) ts)


class FindUpsertElem (a :: Maybe Nat) where
  upsertElem :: Maybe Int

-- # FindUpsertNothing
instance FindUpsertElem 'Nothing where
  upsertElem = Nothing

-- # FindUpsertJust
instance KnownNat n => FindUpsertElem ('Just n) where
  upsertElem =
    Just . fromIntegral . natVal $ Proxy @n

type UpsertElem (key :: Symbol)
                (t :: k)
                (ts :: [(Symbol, k)]) =
  FromMaybe ('(key, t) ': ts)
    =<< Map (Placeholder1Of3 SetIndex '(key, t) ts)  -- ! 1
    =<< FindIndex (TyEq key <=< Fst) ts

upsert
    :: forall key ts t f
     . FindUpsertElem (UpsertLoc key ts)
    => Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f (Eval (UpsertElem key t ts))
upsert k ft (OpenProduct v) =
  OpenProduct $ case upsertElem @(UpsertLoc key ts) of
    Nothing -> V.cons (Any ft) v
    Just n  -> v V.// [(n, Any ft)]

type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem :: forall key ts. KnownNat (FindElem key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)

type LookupType (key :: k) (ts :: [(k, t)]) =
  FromMaybe Stuck =<< Lookup key ts

get
    :: forall key ts f
     . KnownNat (FindElem key ts)
    => Key key
    -> OpenProduct f ts
    -> f (Eval (LookupType key ts)) -- ! 1
get _ (OpenProduct v) =
    unAny $ V.unsafeIndex v $ findElem @key @ts
  where
    unAny (Any a) = unsafeCoerce a  -- ! 2


type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
  SetIndex (FindElem key ts) '(key, t) ts


type family FriendlyFindElem (funcName :: Symbol)
                             (key :: Symbol)
                             (ts :: [(Symbol, k)]) where
  FriendlyFindElem funcName key ts =
    Eval (
      FromMaybe
           ( TypeError
           ( 'Text "Attempted to call `"
       ':<>: 'Text funcName
       ':<>: 'Text "' with key `"
       ':<>: 'Text key
       ':<>: 'Text "'."
       ':$$: 'Text "But the OpenProduct only has keys :"
       ':$$: 'Text "  "
       ':<>: 'ShowType (Eval (Map Fst ts))
           )) =<< FindIndex (TyEq key <=< Fst) ts)

type family ShowList (ts :: [k]) where
  ShowList '[] = Text ""
  ShowList (a ': '[]) = ShowType a
  ShowList (a ': as)  =
    ShowType a ':<>: Text ", " ':<>: ShowList as

type family FriendlyFindElem2 (funcName :: Symbol)
                             (key :: Symbol)
                             (ts :: [(Symbol, k)]) where
  FriendlyFindElem2 funcName key ts =
    Eval (
      FromMaybe
           ( TypeError
           ( 'Text "Attempted to call `"
       ':<>: 'Text funcName
       ':<>: 'Text "' with key `"
       ':<>: 'Text key
       ':<>: 'Text "'."
       ':$$: 'Text "But the OpenProduct only has keys :"
       ':$$: 'Text "  "
       ':<>: ShowList (Eval (Map Fst ts))
           )) =<< FindIndex (TyEq key <=< Fst) ts)


friendlyUpdate
    :: forall key ts t f
     . ( KnownNat (FriendlyFindElem "friendlyUpdate" key ts)
       , KnownNat (FindElem key ts)
       )
    => Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f (Eval (UpdateElem key t ts))
friendlyUpdate _ ft (OpenProduct v) =
  OpenProduct $ v V.// [(findElem @key @ts, Any ft)]

update
    :: forall key ts t f
     . KnownNat (FindElem key ts)
    => Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) =
  OpenProduct $ v V.// [(findElem @key @ts, Any ft)]


type DeleteElem key = Filter (Not <=< TyEq key <=< Fst)

delete
    :: forall key ts f
     . KnownNat (FindElem key ts)
    => Key key
    -> OpenProduct f ts
    -> OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) =
  let (a, b) = V.splitAt (findElem @key @ts) v
   in OpenProduct $ a V.++ V.tail b

friendlyDelete
    :: forall key ts f
     . ( KnownNat (FriendlyFindElem "friendlyDelete" key ts)
       , KnownNat (FindElem key ts)
       )
    => Key key
    -> OpenProduct f ts
    -> OpenProduct f (Eval (DeleteElem key ts))
friendlyDelete _ (OpenProduct v) =
  let (a, b) = V.splitAt (findElem @key @ts) v
   in OpenProduct $ a V.++ V.tail b


peel
    :: forall f name t ts
     . OpenProduct f ('(name, t) ': ts)
    -> (f t, OpenProduct f ts)
peel z@(OpenProduct v) =
  ( get (Key @name) z
  , OpenProduct $ V.tail v
  )


instance Eq (OpenProduct f '[]) where
  _ == _ = True

instance (Eq (f t), Eq (OpenProduct f ts))
      => Eq (OpenProduct f ('(name, t) ': ts)) where
  a == b = peel a == peel b

