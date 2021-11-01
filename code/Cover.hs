{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Cover where

import Diagrams.TwoD.Layout.Grid
import Diagrams.TwoD.Arrow
import Diagrams.Backend.SVG.CmdLine
import Prelude hiding (cycle)
import Diagrams.Prelude hiding (trace, showTrace)
import GHC.Generics
import Control.Monad.Cont
import Data.Void
import Debug.Trace

showTrace :: Show a => a -> a
showTrace = trace =<< show

data Star

class GDrawType (a :: k) where
  gDrawType :: Depth -> Int -> String -> Diagram B

newtype Depth = Depth Int
  deriving Num

instance GDrawType Star where
  gDrawType _ _ _ =
    (centerXY $ frame 0.1 $ triangle 0.1 # fc black # reflectY
                 <> triangle 0.1 # fc black)


instance GDrawType U1 where
  gDrawType _ _ _ = circle 0.08 # fc black

instance GDrawType V1 where
  gDrawType _ _ _ = centerXY $ square 0.16

instance (GDrawType f) => GDrawType (M1 _1 _2 f) where
  gDrawType = gDrawType @_ @f

instance (GDrawType f, GDrawType g) => GDrawType (f :*: g) where
  gDrawType d p x =
    precs d 7 p $ centerXY $
      gDrawType @_ @f (d - 1) 7 ('1' : x)
      |||
      strutX 0.15
      |||
      centerXY (text "×" # fontSizeL 0.3)
      |||
      strutX 0.15
      |||
      gDrawType @_ @g (d - 1) 7 ('2' : x)

instance (GDrawType f, GDrawType g) => GDrawType (f :+: g) where
  gDrawType d p x =
    precs d 6 p $ centerXY $
      gDrawType @_ @f (d - 1) 6 ('L' : x)
      ===
      strutY 0.275
      ===
      centerXY (text "+" # fontSizeL 0.3)
      ===
      strutY 0.275
      ===
      gDrawType @_ @g (d - 1) 6 ('R' : x)



-- instance (GDrawType f, GDrawType g) => GDrawType (f :+: g) where
--   gDrawType d p x = precs d 6 p $ centerXY $
--     if showTrace (width f + width g) > 0.5
--        then
--           f
--           ===
--           strutY 0.275
--           ===
--           centerXY (text "+" # fontSizeL 0.3)
--           ===
--           strutY 0.275
--           ===
--           g
--        else
--           f
--           |||
--           strutX 0.15
--           |||
--           centerXY (text "+" # fontSizeL 0.3)
--           |||
--           strutX 0.15
--           |||
--           g
--     where
--       f = gDrawType @_ @f (d - 1) 6 ('L' : x)
--       g = gDrawType @_ @g (d - 1) 6 ('R' : x)


instance (GDrawType a) => GDrawType (K1 _1 a) where
  gDrawType = gDrawType @_ @a

instance (GDrawType a, GDrawType b) => GDrawType (a -> b) where
  gDrawType d p x =
    let aName = 'A' : x
        bName = 'B' : x
     in precs d 0 p $ connectPerim aName bName (0 @@ deg) (90 @@ deg) $
          gDrawType @_ @a (d - 1) 1 aName
          ===
          strutY 0.1
          ===
          rect 0.005 0.35 # fc black
          ===
          triangle 0.05 # fc black # reflectY
          ===
          strutY 0.2
          ===
          gDrawType @_ @b (d - 1) 0 bName

precs :: Depth -> Int -> Int -> Diagram B -> Diagram B
precs (Depth 0) _ _ _ = mempty
precs _ a b p | a >= b = p
              | otherwise = labeled p

labeled :: Diagram B -> Diagram B
labeled d =
  let d' = scale (3/4) d
   in centerXY
    . frame 0.01
    $ d' # center
      <> rect (width d' + 0.25) (height d' + 0.25)


instance GDrawType Bool where
  gDrawType _ _ _ = mathBB 'B'

mathBB :: Char -> Diagram B
mathBB c = strutY 0.15 ===
    (text (pure c) # fontSizeL 0.4 <> strutX 0.25 <> strutY 0
    <>
     (strutX 0.11 ||| (strutY 0 === text (pure c) # fontSizeL 0.4)))

instance GDrawType Int where
  gDrawType _ _ _ = mathBB 'Z'

instance GDrawType a => GDrawType [a] where
  gDrawType d p x = labeled $ gDrawType @_ @a d p x


instance {-# OVERLAPPABLE #-} GDrawType (Rep f) => GDrawType f where
  gDrawType = gDrawType @_ @(Rep f)

type family Stuck :: a

data Fix f = Fix (f (Fix f))
  deriving (Generic)

data Free f a = Pure a
              | Free (f (Free f a))
  deriving (Generic, Generic1)

data Cofree f a = Cofree a (f (Cofree f a))
  deriving (Generic, Generic1)

data Tree b a = Leaf b | Branch (Tree b a) (Tree b a)
  deriving (Generic, Generic1)

data Foo = Foo Bool Bool | Bar
  deriving Generic

data Moore a b = Moore b (a -> Moore a b)
  deriving Generic

deriving instance Generic (ContT r m a)

main = mainWith (gDrawType @_
  @(( Either (Int -> Cofree ((->) Int) (Maybe Bool))
             (Either (Bool -> Cofree ((->) Bool) (Maybe Int))
                     (Star -> Free ((->) Star) Star
                     , Star -> Free ((->) Star) Star
                     )
             )
    , Cofree (ContT (Int, Maybe Void) (Free (Tree Bool))) Bool
    )
  -> Fix (Tree Bool)
   ) 11 0 "" :: Diagram B)

-- main = mainWith (gDrawType @_ @[Free ((,) (Maybe Int, Int)) Bool -> Cofree ((->) Int) (Bool, Maybe Bool) -> Int] 15 0 "" :: Diagram B)
