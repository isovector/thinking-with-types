-- # pragmas
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Linear where

-- # imports
import           Control.Monad.Indexed
import           Data.Coerce
import           Fcf
import           GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL
import           IxMonad
import           Language.Haskell.DoNotation
import           Prelude hiding (Monad (..), pure)
import qualified System.IO as SIO
import           System.IO hiding (openFile, Handle)


type IsOpen (key :: k) (ts :: [k])
  = IsJust =<< Find (TyEq key) ts

type Close (key :: k) (ts :: [k])
  = Filter (Not <=< TyEq key) ts

newtype Handle s key = Handle
  { unsafeGetHandle :: SIO.Handle
  }

newtype Linear s (i :: LinearState)
                 (j :: LinearState) a = Linear
  { unsafeRunLinear :: Ix IO i j a
  }
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad)

data LinearState = LinearState
  { linearNextKey  :: Nat
  , linearOpenKeys :: [Nat]
  }

runLinear
    :: ( forall s
       . Linear s ('LinearState 0 '[])
                  ('LinearState n '[]) a
       )
    -> IO a
runLinear = coerce

openFile
    :: FilePath
    -> IOMode
    -> Linear s ('LinearState next open)  -- ! 1
                ('LinearState
                    (next TL.+ 1)  -- ! 2
                    (next ': open))  -- ! 3
                (Handle s next)
openFile = coerce SIO.openFile

closeFile
    :: Eval (IsOpen key open) ~ 'True
    => Handle s key
    -> Linear s ('LinearState next open)
                ('LinearState next (Eval (Close key open)))
                ()
closeFile = coerce SIO.hClose

