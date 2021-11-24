-- # pragmas
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Algebra where

import Data.Void
import Data.Word

import Control.Monad (join, guard)
import Data.Maybe (listToMaybe, isJust)

{-

data Void

-- # Unit
data () = ()

data Bool = False | True

to   :: s -> t
from :: t -> s

data Deal a b
  = This a
  | That b
  | TheOther Bool

data Maybe a
  = Nothing
  | Just a

voidUnit :: () -> (Void -> a)
voidUnit _ v = absurd v

-}

data MixedFraction a = Fraction
  { mixedBit    :: Word8
  , numerator   :: a
  , denominator :: a
  }

data Spin = Up | Down

spinToBool1 :: Spin -> Bool
spinToBool1 Up   = False
spinToBool1 Down = True

boolToSpin1 :: Bool -> Spin
boolToSpin1 False = Up
boolToSpin1 True  = Down

spinToBool2 :: Spin -> Bool
spinToBool2 Up   = True
spinToBool2 Down = False

boolToSpin2 :: Bool -> Spin
boolToSpin2 False = Down
boolToSpin2 True  = Up

sumUnitTo :: Either a Void -> a
sumUnitTo (Left a)  = a
sumUnitTo (Right v) = absurd v  -- ! 1

sumUnitFrom :: a -> Either a Void
sumUnitFrom = Left

prodUnitTo :: a -> (a, ())
prodUnitTo a = (a, ())

prodUnitFrom :: (a, ()) -> a
prodUnitFrom (a, ()) = a

productRule1To
    :: (b -> a)
    -> (c -> a)
    -> Either b c
    -> a
productRule1To f _ (Left b)  = f b
productRule1To _ g (Right c) = g c

productRule1From
    :: (Either b c -> a)
    -> (b -> a, c -> a)
productRule1From f = (f . Left, f . Right)

productRule2To
    :: (c -> (a, b))
    -> (c -> a, c -> b)
productRule2To f = (fst . f, snd . f)

productRule2From
    :: (c -> a)
    -> (c -> b)
    -> c
    -> (a, b)
productRule2From f g c = (f c, g c)

uncurry :: (c -> b -> a) -> (b, c) -> a
uncurry f (b, c) = f c b

curry :: ((b, c) -> a) -> c -> b -> a
curry f c b = f (b, c)

data Three = One | Two | Three
  deriving (Eq, Ord, Enum, Bounded)

data TicTacToe a = TicTacToe
  { topLeft   :: a
  , topCenter :: a
  , topRight  :: a
  , midLeft   :: a
  , midCenter :: a
  , midRight  :: a
  , botLeft   :: a
  , botCenter :: a
  , botRight  :: a
  }

data TicTacToe2 a = TicTacToe2
  { board :: Three -> Three -> a
  }

checkWinner :: TicTacToe (Maybe Bool) -> Maybe Bool
checkWinner (TicTacToe {..}) = join $ listToMaybe $ do
  line <- [ [topLeft,   topCenter, topRight]
          , [midLeft,   midCenter, midRight]
          , [botLeft,   botCenter, botRight]
          , [topLeft,   midLeft,   botLeft]
          , [topCenter, midCenter, botCenter]
          , [topRight,  midRight,  botRight]
          , [topLeft,   midCenter, botRight]
          , [topRight,  midCenter, botLeft]
          ]
  let [one, two, three] = line
  guard $ isJust one
       && two == one
       && three == one
  pure one

emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard =
  TicTacToe
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing Nothing

emptyBoard2 :: TicTacToe2 (Maybe Bool)
emptyBoard2 =
  TicTacToe2 $ const $ const Nothing

