{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE UndecidableInstances #-}

module War.Ecstasy where

{-

import           Control.Applicative (Alternative ((<|>)))
import           Control.Lens (Lens', (^.), (?~), (.~), (%~), at, (<>~))
import           Control.Lens (view)
import           Data.Bool
import           Data.Coerce
import           Data.Function
import           Data.Functor.Barbie
import           Data.Generic.HKD
import           Data.Generics.Product.Fields (field', HasField')
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import           Data.Monoid (Endo (..))
import           Data.Ord
import           Data.Proxy
import           GHC.Generics
import           GHC.OverloadedLabels
import           GHC.TypeLits
import           Witherable


newtype Entity w = Entity (HKD w Maybe)
  deriving (Generic)

instance Eq (HKD w Maybe) => Eq (Entity w) where
  Entity a == Entity b = a == b

instance Ord (HKD w Maybe) => Ord (Entity w) where
  Entity a `compare` Entity b = a `compare` b


data System w = System
  { systemData  :: HKD w IntMap
  , systemAlive :: IntSet
  , systemUniq  :: Id
  } deriving (Generic)


newSystem :: Monoid (HKD w IntMap) => System w
newSystem = System mempty mempty $ Id 0


numEntities :: System w -> Int
numEntities = idToInt . systemUniq


data Query w a where
  RefineMap  :: (a -> Maybe b) -> Query w a -> Query w b
  Const      :: a -> Query w a
  Ap         :: Query w (a -> b) -> Query w a -> Query w b
  With       :: Component w a -> Query w a
  Without    :: Component w a -> Query w ()
  Together   :: Query w a -> Query w b -> Query w (a, b)
  Alt        :: Query w a -> Query w b -> Query w (Either a b)
  Try        :: Query w a -> Query w (Maybe a)
  Fail       :: Query w a
  UniqId     :: Query w Id
  Particular :: Id -> Query w ()
  Subquery   :: Query w a -> Query w [a]
  Everything :: Query w (Entity w)

{-

emap :: IdTarget w m -> QueryT w m (Setter w) -> SystemT w m ()

efor :: IdTarget w m -> QueryT w m  -> SystemT w m [a]

allIds  :: IdTarget w m
someIds :: [Id] -> IdTarget w m
anId    :: Id -> IdTarget w m
idsWith :: (System w -> a) -> IdTarget w m

newtype QueryT w m a = QueryT
  { runQueryT' :: ReaderT (Id, System w) (MaybeT m) a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , Alternative
           )

runQueryT :: Monad m => Id -> QueryT w m a -> SystemT w m (Maybe a)

-}

-- # QueryFunctor
instance Functor (Query w) where
  fmap f = refineMap (Just . f)

-- # QueryApplicative
instance Applicative (Query w) where
  pure  = Const
  (<*>) = Ap

-- # QueryFilterable
instance Filterable (Query w) where
  mapMaybe = refineMap


data Component w a = Component
  { compName   :: String
  , compEntity :: Lens' w a
  , compSystem :: Lens' (System w) (IntMap a)
  }

instance Eq (Component w a) where
  (==) = (==) `on` compName

instance Ord (Component w a) where
  compare = comparing compName

instance
    ( KnownSymbol nm
    , HasField' nm w a
    , HasField' nm (HKD w IntMap) (IntMap a)
    ) => IsLabel nm (Component w a) where
  fromLabel =
    Component
      (symbolVal $ Proxy @nm)
      (field' @nm)
      (field' @"systemData" . field' @nm)


data Setter w where
  Set       :: Component w a -> a -> Setter w
  Unchanged :: Setter w
  Unset     :: Component w a -> Setter w
  Delete    :: Setter w
  Both      :: Setter w -> Setter w -> Setter w


newtype Id = Id
  { idToInt :: Int
  } deriving (Show, Eq, Generic, Ord)


findRelevant :: System w -> Query w a -> IntSet
findRelevant sys (RefineMap _ q)
  = findRelevant sys q
findRelevant sys (Const _)
  = mkAllIntSet sys
findRelevant sys (Ap qf qa)
  = IS.intersection (findRelevant sys qf)
                    (findRelevant sys qa)
findRelevant sys (With c)
  = IM.keysSet
  $ view (compSystem c) sys
findRelevant sys (Without c)
  = IS.difference (systemAlive sys)
  $ findRelevant sys
  $ With c
findRelevant sys (Together a b)
  = IS.intersection (findRelevant sys a) (findRelevant sys b)
-- ...

findRelevant sys UniqId
  = mkAllIntSet sys
findRelevant _ (Particular ix)
  = IS.singleton $ idToInt ix
findRelevant sys (Try _)
  = mkAllIntSet sys
findRelevant sys (Subquery _)
  = mkAllIntSet sys
findRelevant sys Everything
  = systemAlive sys
findRelevant sys (Alt a b)
  = IS.union (findRelevant sys a) (findRelevant sys b)
findRelevant _ Fail
  = mempty


mkAllIntSet :: System w -> IntSet
mkAllIntSet
  = IS.fromList
  . enumFromTo 0
  . subtract 1
  . idToInt
  . systemUniq


constantValue :: Query w a -> Maybe a
constantValue (RefineMap f q)
  = f =<< constantValue q
constantValue (Const c) = Just c
constantValue (Ap qf qa)
  = constantValue qf <*> constantValue qa
constantValue (With _) = Nothing
constantValue (Without _) = Nothing
constantValue (Together q1 q2)
  = (,) <$> constantValue q1
        <*> constantValue q2
-- ...

constantValue UniqId = Nothing
constantValue (Particular _) = Nothing
constantValue (Alt q1 q2) = fmap Left (constantValue q1) <|> fmap Right (constantValue q2)
constantValue (Try q) = Just <$> constantValue q
constantValue (Subquery _) = Nothing
constantValue Everything = Nothing
constantValue Fail = Nothing


createEntity
    :: (FunctorB (HKD w), Monoid (HKD w IntMap))
    => Entity w
    -> System w
    -> (Id, System w)
createEntity (Entity e) w =
  let ix = idToInt $ systemUniq w
      ix' = Id $ ix + 1
      e' = bmap (maybe mempty (IM.singleton ix)) e
   in ( Id ix
      , w & field' @"systemData" <>~ e'
          & field' @"systemAlive" %~ IS.insert ix
          & field' @"systemUniq" .~ ix'
      )

delEntity :: FunctorB (HKD w) => Id -> System w -> System w
delEntity ix = setEntity ix Delete


getEntity :: FunctorB (HKD w) => Id -> System w -> Entity w
getEntity ix w = Entity $ bmap (IM.lookup $ idToInt ix) $ systemData w


queryEntity :: (FunctorB (HKD w), Generic w) => Id -> Query w a -> System w -> Maybe a
queryEntity ix (Const a) s
  = bool Nothing (Just a) $ isBounded ix s
queryEntity ix (Subquery q) s
  = bool Nothing (Just $ query q s) $ isBounded ix s
queryEntity ix (RefineMap f q) s
  = f =<< queryEntity ix q s
queryEntity ix (With c) s
  = s ^. compAtIx c ix
queryEntity ix (Without c) s
  = maybe (Just ()) (const Nothing) $ s ^. compAtIx c ix
queryEntity ix (Together c1 c2) s
  = (,)
      <$> queryEntity ix c1 s
      <*> queryEntity ix c2 s
queryEntity ix (Ap c1 c2) s
  = queryEntity ix c1 s <*> queryEntity ix c2 s
queryEntity ix UniqId s
  = bool Nothing (Just ix) $ isBounded ix s
queryEntity ix (Particular ix') sys
  = bool Nothing (Just ())
  $ ix == ix' && IS.member (idToInt ix) (systemAlive sys)
queryEntity ix Everything s
  = Just $ getEntity ix s
queryEntity ix (Try q) s
  = bool Nothing (Just $ queryEntity ix q s) $ isBounded ix s
queryEntity ix (Alt q1 q2) s = do
  a <- queryEntity ix (try q1) s
  case a of
    Just a' -> pure $ Left a'
    Nothing -> fmap Right $ queryEntity ix q2 s
queryEntity _ Fail _ = Nothing

isBounded :: Id -> System w -> Bool
isBounded ix s = idToInt ix >= 0 && idToInt ix < idToInt (systemUniq s)

compAtIx
    :: Functor f
    => Component w a
    -> Id
    -> (Maybe a -> f (Maybe a))
    -> System w -> f (System w)
compAtIx c ix = compSystem c . at (idToInt ix)

setEntity :: FunctorB (HKD w) => Id -> Setter w -> System w -> System w
setEntity _ Unchanged sys = sys
setEntity ix _ sys | not (isBounded ix sys) = sys
setEntity ix Delete sys =
  sys
    & field' @"systemData" %~ bmap (IM.delete (idToInt ix))
    & field' @"systemAlive" %~ IS.delete (idToInt ix)
setEntity ix (Unset c) sys =
  sys
    & compAtIx c ix .~ Nothing
setEntity ix (Both s1 s2) sys =
  setEntity ix s2 $ setEntity ix s1 sys
setEntity ix (Set c a) sys =
  sys
    & compAtIx c ix ?~ a

query :: (FunctorB (HKD w), Generic w) => Query w a -> System w -> [a]
query (Subquery q) s = [query q s]
query UniqId s = coerce $ enumFromTo 0 $ numEntities s - 1
query q s
  = mapMaybe (\ix -> queryEntity ix q s)
  . coerce
  . IS.toList
  $ findRelevant s q

update :: (FunctorB (HKD w), Generic w) => Query w a -> (a -> Setter w) -> System w -> System w
update q f s
  = flip appEndo s
  . foldMap (\(ix, a) -> Endo $ setEntity ix (f a))
  $ query (Together UniqId q) s

particular :: Id -> Query w ()
particular = Particular

uniqId :: Query w Id
uniqId = UniqId

with :: Component w a -> Query w a
with = With

fetch :: Component w a -> Query w (Maybe a)
fetch = try . with

without :: Component w a -> Query w ()
without = Without

refine :: (a -> Bool) -> Query w a -> Query w a
refine p = refineMap (bool Nothing . Just <*> p)

refineMap :: (a -> Maybe b) -> Query w a -> Query w b
refineMap = RefineMap

together :: Query w a -> Query w b -> Query w (a, b)
together (Const a) (Const b) = Const (a, b)
together q1 q2 = Together q1 q2

eitherQ :: Query w a -> Query w b -> Query w (Either a b)
eitherQ = Alt

try :: Query w a -> Query w (Maybe a)
try (Const c) = Const (Just c)
try q = Try q

subquery  :: Query w a -> Query w [a]
subquery = Subquery

everything :: Query w (Entity w)
everything = Everything

set :: Component w a -> a -> Setter w
set = Set

unset :: Component w a -> Setter w
unset = Unset

unchanged :: Setter w
unchanged = Unchanged

both :: Setter w -> Setter w  -> Setter w
both Delete _ = Delete
both _ Delete = Delete
both Unchanged a = a
both a Unchanged = a
both (Unset c) s@(Unset c')
  | compName c == compName c'
  = s
both (Unset c) s@(Set c' _)
  | compName c == compName c'
  = s
both (Set c' _) s@(Unset c)
  | compName c == compName c'
  = s
both (Set c _) s@(Set c' _)
  | compName c == compName c'
  = s
both s1 s2 = Both s1 s2

delete :: Setter w
delete = Delete

-}
