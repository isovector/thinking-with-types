{-# LANGUAGE TypeFamilies #-}

module HKD where

import GHC.Generics
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)


type family HKD (f :: Type -> Type)
                (a :: Type) :: Type where
  HKD Identity a = a
  HKD f        a = f a

data Foo f = Foo
  { bar :: HKD f Int
  }

-- # eqInstFoo
deriving instance Eq (Foo Identity)


class GFlay f i o where
  gflay :: i x -> f (o x)

-- # gflayK1
instance Functor f => GFlay f (K1 _1 (f a)) (K1 _1 a) where
  gflay (K1 a) = fmap K1 a

-- # gflayU1
instance Applicative f => GFlay f U1 U1 where
  gflay U1 = pure U1

-- # gflayV1
instance GFlay f V1 V1 where
  gflay _ = error "absurd"

-- # gflayTimes
instance (Applicative f, GFlay f i o, GFlay f i' o')
      => GFlay f (i :*: i') (o :*: o') where
  gflay (a :*: b) = (:*:) <$> gflay a <*> gflay b

-- # gflayPlus
instance (Applicative f, GFlay f i o, GFlay f i' o')
      => GFlay f (i :+: i') (o :+: o') where
  gflay (L1 a) = fmap L1 $ gflay a
  gflay (R1 b) = fmap R1 $ gflay b

-- # gflayM1
instance (Applicative f, GFlay f i o)
      => GFlay f (M1 _1 _2 i) (M1 _1 _2 o) where
  gflay (M1 a) = fmap M1 $ gflay a

flay
    :: ( Generic (hkd f)
       , Generic (hkd Identity)
       , GFlay f (Rep (hkd f))
                 (Rep (hkd Identity))
       , Applicative f
       )
    => hkd f
    -> f (hkd Identity)
flay = fmap to . gflay . from

