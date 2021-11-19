-- # pragmas
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenProduct where

-- # imports
import           Data.Constraint
import           Data.Kind (Type)
import           Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import           Fcf hiding (Any)
import           GHC.OverloadedLabels (IsLabel (..))
import           GHC.TypeLits
import           Unsafe.Coerce (unsafeCoerce)


data Any (f :: k -> Type) where
  Any :: f t -> Any f


type OpenProduct
    :: (k -> Type)
    -> [(Symbol, k)]
    -> Type
-- TODO(sandy): this annotation is probably wrong
data OpenProduct f ts where  -- ! 1
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty


data Key (key :: Symbol) = Key


-- # keyIsLabel
instance (key ~ key') -- ! 1
      => IsLabel key (Key key') where
  fromLabel = Key

type UniqueKey :: k -> [(k, t)] -> Exp Bool
type UniqueKey key ts
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

type RequireUniqueKey
    :: Bool
    -> Symbol
    -> k
    -> [(Symbol, k)]
    -> Constraint
type family RequireUniqueKey result key t ts where
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

type UpsertLoc
    :: Symbol
    -> [(Symbol, k)]
    -> Maybe Nat
type UpsertLoc key ts =
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

type UpsertElem
    :: Symbol
    -> k
    -> [(Symbol, k)]
    -> Exp [(Symbol, k)]
type UpsertElem key t ts =
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
upsert _ ft (OpenProduct v) =
  OpenProduct $ case upsertElem @(UpsertLoc key ts) of
    Nothing -> V.cons (Any ft) v
    Just n  -> v V.// [(n, Any ft)]

type FindElem
    :: Symbol
    -> [(Symbol, k)]
    -> Nat
type FindElem key ts =
  Eval (FromMaybe Stuck
    =<< FindIndex (TyEq key <=< Fst) ts)

findElem
    :: forall key ts
     . KnownNat (FindElem key ts)
    => Int
findElem = fromIntegral
         . natVal
         $ Proxy @(FindElem key ts)

type LookupType
    :: k
    -> [(k, t)]
    -> Exp t
type LookupType key ts =
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


type UpdateElem
    :: Symbol
    -> k
    -> [(Symbol, k)]
    -> Exp [(Symbol, k)]
type UpdateElem key t ts =
  SetIndex (FindElem key ts) '(key, t) ts


type FriendlyFindElem :: Symbol -> Symbol -> [(Symbol, k)] -> k
type family FriendlyFindElem funcName key ts where
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

type ShowList :: [k] -> ErrorMessage
type family ShowList ts where
  ShowList '[] = 'Text ""
  ShowList (a ': '[]) = 'ShowType a
  ShowList (a ': as)  =
    'ShowType a ':<>: 'Text ", " ':<>: ShowList as

type FriendlyFindElem2 :: Symbol -> Symbol -> [(Symbol, k)] -> k
type family FriendlyFindElem2 funcName key ts where
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

