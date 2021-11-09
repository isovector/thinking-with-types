{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.HKDNames where

import Data.Kind
import GHC.Generics
import Generics.Kind
import Generics.Kind.TH
import Data.Functor.Const
import GHC.TypeLits


data Person f = Person
  { personAge  :: f Int
  , personName :: f String
  }
  deriving Generic

deriving instance (Show (f Int), Show (f String)) => Show (Person f)

deriveGenericK ''Person

type HKDKind = (Type -> Type) -> Type

type GNames :: (LoT HKDKind -> Type) -> Constraint
class GNames f where
  gnames :: f (LoT1 (Const String))

instance (GNames f, GNames g)
      => GNames (f :*: g)
         where
  gnames = gnames :*: gnames

instance GNames f
      => GNames (C1 _1 f)
         where
  gnames = M1 gnames

instance GNames f
      => GNames (D1 _1 f)
         where
  gnames = M1 gnames

instance KnownSymbol name
      => GNames (S1 ('MetaSel ('Just name) _1 _2 _3)
                    (Field (Var0 ':@: a)))
         where
  gnames = M1 $ Field $ Const $ symbolVal $ Proxy @name

instance GNames U1 where
  gnames = U1


personNames :: Person (Const String)
personNames = toK gnames


