-- # pragmas
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# OPTIONS_GHC -Wall #-}

module JSONSchema where

-- # imports
import Control.Monad.Writer
import Data.Aeson (Value (..), (.=), object)
import Data.Kind (Constraint, Type)
import Data.Text (Text, pack)
import Data.Typeable
import Data.Vector (fromList)
import GHC.Generics
import GHC.TypeLits
import GHC.TypeLits qualified as Err


data Person = Person
  { name        :: String
  , age         :: Int
  , phone       :: Maybe String
  , permissions :: [Bool]
  }
  deriving (Generic)


type GSchema :: (Type -> Type) -> Constraint
class GSchema a where
  gschema :: Writer [Text] Value

makePropertyObj
    :: forall name
     . (KnownSymbol name)
    => Value -> Value
makePropertyObj = undefined -- object
  -- [ pack (symbolVal $ Proxy @name) .= v
  -- ]

makeTypeObj
    :: forall a
     . KnownSymbol (ToJSONType a)
    => Value
makeTypeObj = object
  [ "type" .=
      String (pack . symbolVal $ Proxy @(ToJSONType a))
  ]

emitRequired
    :: forall nm
     . KnownSymbol nm
    => Writer [Text] ()
emitRequired = tell . pure . pack . symbolVal $ Proxy @nm

type ToJSONType :: Type -> Symbol
type family ToJSONType t where
  ToJSONType Int     = "integer"
  ToJSONType Integer = "integer"
  ToJSONType Float   = "number"
  ToJSONType Double  = "number"
  ToJSONType String  = "string"
  ToJSONType Bool    = "boolean"
  ToJSONType [t]     = "array"
  ToJSONType t       = TypeName t

type TypeName t = RepName (Rep t)

type RepName :: (Type -> Type) -> Symbol
type family RepName x where
  RepName (D1 ('MetaData nm _ _ _) _) = nm

-- # gschemaMaybe
instance {-# OVERLAPPING #-}
  ( KnownSymbol nm
  , KnownSymbol (ToJSONType a)
  )
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3)
                     (K1 _4 (Maybe a))) where
  gschema = pure
          . makePropertyObj @nm
          $ makeTypeObj @a
  {-# INLINE gschema #-}

-- # gschemaString
instance {-# OVERLAPPING #-} KnownSymbol nm
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3)
                     (K1 _4 String)) where
  gschema = do
    emitRequired @nm
    pure . makePropertyObj @nm
         $ makeTypeObj @String
  {-# INLINE gschema #-}

-- # gschemaList
instance {-# OVERLAPPING #-}
  ( KnownSymbol nm
  , KnownSymbol (ToJSONType [a])
  , KnownSymbol (ToJSONType a)
  )
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3)
                     (K1 _4 [a])) where
  gschema = do
    emitRequired @nm
    let innerType = object
          [ "items" .= makeTypeObj @a
          ]
    pure . makePropertyObj @nm
         . mergeObjects innerType
         $ makeTypeObj @[a]
  {-# INLINE gschema #-}

-- # gschemaK1
instance (KnownSymbol nm, KnownSymbol (ToJSONType a))
    => GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3)
                     (K1 _4 a)) where
  gschema = do
    emitRequired @nm  -- ! 1
    pure . makePropertyObj @nm  -- ! 2
         $ makeTypeObj @a
  {-# INLINE gschema #-}

mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object $ a <> b
mergeObjects _ _ = error "unsafe use of mergeObjects"

-- # gschemaTimes
instance (GSchema f, GSchema g)
      => GSchema (f :*: g) where
  gschema =
    mergeObjects <$> gschema @f
                 <*> gschema @g
  {-# INLINE gschema #-}

-- # gschemaM1C
instance GSchema a => GSchema (M1 C _1 a) where
  gschema = gschema @a
  {-# INLINE gschema #-}

-- # gschemaM1D
instance (GSchema a, KnownSymbol nm)
    => GSchema (M1 D ('MetaData nm _1 _2 _3) a) where
  gschema = do
    sch <- gschema @a
    pure $ object
      [ "title" .= (String . pack . symbolVal $ Proxy @nm)
      , "type"  .= String "object"
      , "properties" .= sch
      ]
  {-# INLINE gschema #-}

-- # gschemaPlus
instance
  (TypeError ('Err.Text
      "JSON Schema does not support sum types"))
    => GSchema (f :+: g) where
  gschema =
    error
      "JSON Schema does not support sum types"
  {-# INLINE gschema #-}

schema
    :: forall a
     . (GSchema (Rep a), Generic a)
    => Value
schema =
  let (v, reqs) = runWriter $ gschema @(Rep a)
   in mergeObjects v $ object
        [ "required" .=
            Array (fromList $ String <$> reqs)
        ]
{-# INLINE schema #-}

