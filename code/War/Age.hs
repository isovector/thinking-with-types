{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module War.Age where

import Data.Kind
import Data.Coerce (coerce)

data Age
  = Current
  | Stale Type

{-

-- # BadAge
data Age = Current | Stale

-}

type Tracked :: Age -> Type -> Type
newtype Tracked age a = UnsafeTracked
  { unTrack :: a
  }

type PositionMap :: Age -> Age -> Type
newtype PositionMap from to = PositionMap
  { getPositionMapping :: PositionMapping
  }

class MapAge a where
  mapAgeFrom
      :: PositionMap from to
      -> Tracked to a
      -> Tracked from a
  mapAgeTo
      :: PositionMap from to
      -> Tracked from a
      -> Tracked to a

-- # MapAgeRange
instance MapAge Range where
  mapAgeFrom = coerce fromCurrentRange
  mapAgeTo = coerce toCurrentRange

data TrackedStale a where
  TrackedStale
      :: Tracked ('Stale s) a
      -> PositionMap ('Stale s) 'Current
      -> TrackedStale a

fromCurrentRange :: PositionMapping -> Range -> Range
toCurrentRange   :: PositionMapping -> Range -> Range


data PositionMapping

data Range

fromCurrentRange = undefined
toCurrentRange = undefined

