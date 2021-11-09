-- # pragmas
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Sigma where

-- # imports
import Data.Aeson
import Data.Constraint
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Ord.Singletons
import Data.Singletons.TH
import Data.String.Singletons
import Prelude.Singletons


data Sigma (f :: k -> Type) where
  Sigma :: Sing a -> f a -> Sigma f


withSigma
    :: (forall (a :: k). Sing a -> f a -> r)
    -> Sigma f
    -> r
withSigma c (Sigma s f) = c s f


toSigma
    :: SingI a
    => f a
    -> Sigma f
toSigma fa = Sigma sing fa


fromSigma
    :: forall k (a :: k) (f :: k -> Type)
     . ( SingI a
       , SDecide k
       )
    => Sigma f
    -> Maybe (f a)
fromSigma (Sigma s f) =
  case s %~ sing @a of
    Proved Refl -> Just f  -- ! 1
    Disproved _ -> Nothing

-- # LogType
singletons [d|
  data LogType
    = JsonMsg
    | TextMsg
    deriving (Eq, Ord, Show)
  |]

data family LogMsg (msg :: LogType)

-- # LogMsgPayloadJson
data instance LogMsg 'JsonMsg = Json Value
  deriving (Eq, Show)

-- # LogMsgPayloadText
data instance LogMsg 'TextMsg = Text String
  deriving (Eq, Show)

logs :: [Sigma LogMsg]
logs =
  [ toSigma $ Text "hello"
  , toSigma $ Json $
      object
        [ "world" .= (5 :: Int) ]
  , toSigma $ Text "structured logging is cool"
  ]

showLogs :: [Sigma LogMsg] -> [String]
showLogs = fmap $ withSigma $ \sa fa ->
  case dict1 @_ @Show @LogMsg sa of
    Dict -> show fa

jsonLogs :: [LogMsg 'JsonMsg]
jsonLogs = catSigmas logs

catSigmas
    :: forall k (a :: k) f
     . (SingI a, SDecide k)
    => [Sigma f]
    -> [f a]
catSigmas = mapMaybe fromSigma

type Dict1
    :: forall k
     . (Type -> Constraint)
    -> (k -> Type)
    -> Constraint
class Dict1 c f where
  dict1 :: Sing a -> Dict (c (f a))

-- # Dict1LogMsgPayload
instance ( c (LogMsg 'JsonMsg)
         , c (LogMsg 'TextMsg)
         ) => Dict1 c LogMsg where
  dict1 SJsonMsg = Dict
  dict1 STextMsg = Dict


-- # ShowSigma
instance ( Dict1 Show (f :: k -> Type)
         , Show (Demote k)
         , SingKind k
         ) => Show (Sigma f) where
  show (Sigma sa fa) =
    case dict1 @_ @Show @f sa of
      Dict -> mconcat
        [ "Sigma "
        , show $ fromSing sa
        , " ("
        , show fa
        , ")"
        ]


-- # EqSigma
instance ( Dict1 Eq (f :: k -> Type)  -- ! 1
         , SDecide k
         ) => Eq (Sigma f) where
  Sigma sa fa == Sigma sb fb =
    case sa %~ sb of
      Proved Refl ->
        case dict1 @_ @Eq @f sa of
          Dict -> fa == fb
      Disproved _ -> False

-- # OrdSigma
instance ( Dict1 Eq  (f :: k -> Type)
         , Dict1 Ord f
         , SDecide k
         , SingKind k
         , Ord (Demote k)
         ) => Ord (Sigma f) where
  compare (Sigma sa fa) (Sigma sb fb) =
    case sa %~ sb of
      Proved Refl ->
        case dict1 @_ @Ord @f sa of
          Dict -> compare fa fb
      Disproved _ ->
        compare (fromSing sa) (fromSing sb)

