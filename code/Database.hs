-- # pragmas
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wall #-}

module Database where

-- # imports
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import           Data.Kind (Type)
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           GHC.Generics
import           Prelude hiding (lookup)


{-

data PersonRow = PersonRow
  { rowName    :: String
  , rowAge     :: Int
  , rowAddress :: Maybe String
  }

data PersonTable = PersonTable
  { tableName    :: Seq String  -- ! 1
  , tableAge     :: Seq Int
  , tableAddress :: IntMap String  -- ! 2
  }

-}

data PersonPattern (r :: Representation) =
  PersonPattern
  { name    :: Column r 'NotNull String
  , age     :: Column r 'NotNull Int
  , address :: Column r 'Null    String
  }
  deriving (Generic)

-- # deriving
deriving instance Show (PersonPattern 'RowRep)
deriving instance Show (PersonPattern 'TableRep)
deriving instance Show (PersonPattern 'UpdateRep)

data Representation
  = RowRep
  | TableRep
  | UpdateRep

type Row   = PersonPattern 'RowRep

type Table = PersonPattern 'TableRep


data Update a
  = Set a
  | Keep
  deriving (Eq, Ord, Show)

data Nullable
  = Null
  | NotNull


type family Column (r :: Representation)
                   (n :: Nullable)
                   (t :: Type) :: Type where
  Column 'RowRep    'NotNull t = t
  Column 'TableRep  'NotNull t = Seq t
  Column 'UpdateRep 'NotNull t = Update t
  Column 'RowRep    'Null t = Maybe t
  Column 'TableRep  'Null t = IntMap t
  Column 'UpdateRep 'Null t = Maybe (Update t)  -- ! 1


class GLookup table row where
  gLookup :: Int -> table x -> Maybe (row x)

-- # GLookupK1IntMap
-- 'TableRep ~> 'RowRep for 'Null
instance GLookup (K1 _1 (IntMap a))
                 (K1 _1 (Maybe a)) where
  gLookup idx (K1 im) =
    Just . K1 $ IM.lookup idx im

-- # GLookupK1Seq
-- 'TableRep ~> 'RowRep for 'NotNull
instance GLookup (K1 _1 (Seq a)) (K1 _1 a) where
  gLookup idx (K1 sq) =
    K1 <$> S.lookup idx sq

-- # GLookupU1
instance GLookup U1 U1 where
  gLookup _ _ = Just U1

-- # GLookupV1
instance GLookup V1 V1 where
  gLookup _ _ = Nothing

-- # GLookupTimes
instance (GLookup a a', GLookup b b')
    => GLookup (a :*: b) (a' :*: b') where
  gLookup idx  (ca :*: cb) =
    (:*:) <$> gLookup idx ca
          <*> gLookup idx cb

-- # GLookupM1
instance (GLookup a b)
    => GLookup (M1 _1 _2 a) (M1 _1 _2 b) where
  gLookup idx (M1 c) =
    M1 <$> gLookup idx c


class GInsert row table where
  gInsert :: Int -> row x -> table x -> table x

-- # GInsertK1IntMap
-- 'RowRep ~> 'TableRep for 'Null
instance GInsert (K1 _1 (Maybe a))
                 (K1 _1 (IntMap a)) where
  gInsert idx (K1 a) (K1 im) =
    K1 $ IM.alter (const a) idx im

-- # GInsertK1Seq
-- 'RowRep ~> 'TableRep for 'NotNull
instance GInsert (K1 _1 a) (K1 _1 (Seq a)) where
  gInsert _ (K1 a) (K1 sq) = K1 $
    sq S.|> a

instance GInsert U1 U1 where
  gInsert _ U1 U1 = U1

instance GInsert V1 V1 where
  gInsert _ v _ = absurdV1 v

absurdV1 :: V1 x -> a
absurdV1 v = case v of {}

instance (GInsert a a', GInsert b b')
    => GInsert (a :*: b) (a' :*: b') where
  gInsert idx (a :*: b) (ca :*: cb) =
    gInsert idx a ca :*: gInsert idx b cb

instance GInsert a b
    => GInsert (M1 _1 _2 a) (M1 _1 _2 b) where
  gInsert idx (M1 a) (M1 c) =
    M1 $ gInsert idx a c

class GUpdate update table where
  gUpdate :: Int -> update x -> table x -> table x

-- # GUpdateK1Maybe
instance GUpdate (K1 _1 (Maybe (Update a)))
                 (K1 _1 (IntMap a)) where
  gUpdate idx (K1 (Just (Set a))) (K1 im) =
    K1 $ IM.alter (const $ Just a) idx im
  gUpdate _   (K1 (Just Keep)) (K1 im) = K1 im
  gUpdate idx (K1 Nothing) (K1 im) =
    K1 $ IM.delete idx im

-- # GUpdateK1Update
instance GUpdate (K1 _1 (Update a))
                 (K1 _1 (Seq a)) where
  gUpdate idx (K1 (Set a)) (K1 sq) =
    K1 $ S.update idx a sq
  gUpdate _ (K1 Keep) (K1 im) =
    K1 im

instance GUpdate U1 U1 where
  gUpdate _ _ _ = U1

instance GUpdate V1 V1 where
  gUpdate _ v _ = absurdV1 v

instance (GUpdate a a', GUpdate b b') => GUpdate (a :*: b) (a' :*: b') where
  gUpdate idx (a :*: b) (ca :*: cb) =
    gUpdate idx a ca :*: gUpdate idx b cb

instance (GUpdate a b) => GUpdate (M1 _1 _2 a) (M1 _1 _2 b) where
  gUpdate idx (M1 a) (M1 c) =
    M1 $ gUpdate idx a c


class GEmpty table where
  gEmpty :: table x

-- # K1GEmpty1
instance GEmpty (K1 _1 (IntMap a)) where
  gEmpty = K1 IM.empty

-- # K1GEmpty2
instance GEmpty (K1 _1 (Seq a)) where
  gEmpty = K1 S.empty

instance (GEmpty a, GEmpty b) => GEmpty (a :*: b) where
  gEmpty = gEmpty :*: gEmpty

instance GEmpty U1 where
  gEmpty = U1

instance GEmpty a => GEmpty (M1 _1 _2 a) where
  gEmpty = M1 gEmpty

empty
    :: ( Generic (pat 'TableRep)
       , GEmpty (Rep (pat 'TableRep))
       )
    => pat 'TableRep
empty = to gEmpty



-- # HKDImpl
class ( Generic (pat a)
      , Generic (pat b)
      , c (Rep (pat a)) (Rep (pat b))
      ) => HKDImpl c pat a b

-- # InstHKDImpl
instance ( Generic (pat a)
         , Generic (pat b)
         , c (Rep (pat a)) (Rep (pat b))
         ) => HKDImpl c pat a b


update
    :: HKDImpl GUpdate pat 'UpdateRep 'TableRep
    => Int
    -> pat 'UpdateRep
    -> pat 'TableRep
    -> pat 'TableRep
update idx u t = to $ gUpdate idx (from u) (from t)


insert
    :: HKDImpl GInsert pat 'RowRep 'TableRep
    => Int
    -> pat 'RowRep
    -> pat 'TableRep
    -> pat 'TableRep
insert idx r t = to $ gInsert idx (from r) (from t)



lookup
    :: HKDImpl GLookup pat 'TableRep 'RowRep
    => Int
    -> pat 'TableRep
    -> Maybe (pat 'RowRep)
lookup idx t = fmap to $ gLookup idx (from t)

{-

-- # dumbLookup
lookup
    :: ( Generic (pat 'TableRep)
       , Generic (pat 'RowRep)
       , GLookup (Rep (pat 'TableRep))
                 (Rep (pat 'RowRep))
       )
    => Int
    -> pat 'TableRep
    -> Maybe (pat 'RowRep)
lookup idx t = fmap to $ gLookup idx (from t)

-}
