{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Data.TimeoutMap
    ( TimeoutMap, toList
    , lookup
    , insert, renew
    , adjust, update
    , delete
    , clean
    , mutateWith
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson
                     ( FromJSON, ToJSON, ToJSONKey, toEncoding
                     , genericToEncoding, defaultOptions
                     )


-- base ----------------------------------------------------------------------
import           Control.Applicative (empty)
import           Data.Bitraversable (bisequence)
import           Data.Functor.Identity (Identity (Identity), runIdentity)
import           Data.Maybe (fromJust, mapMaybe)
import           Data.Semigroup (Semigroup)
import           Data.Tuple (swap)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Prelude hiding (lookup)


-- bifunctors ----------------------------------------------------------------
import           Data.Biapplicative (biliftA2)


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData)


-- hashable ------------------------------------------------------------------
import           Data.Hashable (Hashable)


-- hashable-time -------------------------------------------------------------
import           Data.Hashable.Time ()


-- semigroupoids -------------------------------------------------------------
import           Data.Functor.Apply (Apply, liftF2)
import           Data.Functor.Alt (Alt, (<!>))
import           Data.Functor.Plus (Plus, zero)


-- time ----------------------------------------------------------------------
import           Data.Time.Clock
                     ( UTCTime, NominalDiffTime, addUTCTime, diffUTCTime
                     )


-- unordered-containers ------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H


------------------------------------------------------------------------------
newtype TimeoutMap k a = TimeoutMap (HashMap k (UTCTime, a))
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable, FromJSON
    , Functor
    )


------------------------------------------------------------------------------
instance (Eq k, Hashable k) => Apply (TimeoutMap k) where
    liftF2 f (TimeoutMap a) (TimeoutMap b) =
        TimeoutMap (liftF2 (biliftA2 max f) a b)


------------------------------------------------------------------------------
instance (Eq k, Hashable k) => Alt (TimeoutMap k) where
    TimeoutMap a <!> TimeoutMap b = TimeoutMap $ a <> b


------------------------------------------------------------------------------
instance (Eq k, Hashable k) => Plus (TimeoutMap k) where
    zero = TimeoutMap H.empty


------------------------------------------------------------------------------
instance (ToJSONKey k, ToJSON a) => ToJSON (TimeoutMap k a) where
    toEncoding = genericToEncoding defaultOptions


------------------------------------------------------------------------------
instance (Eq k, Hashable k, Semigroup a) => Semigroup (TimeoutMap k a) where
    TimeoutMap x <> TimeoutMap y = TimeoutMap $ H.unionWith go x y
      where
        go = biliftA2 max (<>)


------------------------------------------------------------------------------
instance (Eq k, Hashable k, Semigroup a) => Monoid (TimeoutMap k a) where
    mempty = TimeoutMap mempty


------------------------------------------------------------------------------
toList :: UTCTime -> TimeoutMap k a -> [(k, a)]
toList now (TimeoutMap as) = mapMaybe go $ H.toList as
  where
    go (k, (t, a)) | t >= now = pure (k, a)
    go _ = empty


------------------------------------------------------------------------------
lookup :: (Eq k, Hashable k)
    => UTCTime -> k -> TimeoutMap k a -> (Maybe (NominalDiffTime, a))
lookup = bisequence . snd .:. mutateWith empty (\a -> (a, pure (empty, a)))


------------------------------------------------------------------------------
insert :: (Eq k, Hashable k)
    => NominalDiffTime -> a
    -> UTCTime -> k -> TimeoutMap k a -> (TimeoutMap k a, NominalDiffTime)
insert t a = fmap fromJust .:. mutate (pure (t, a)) (\_ -> pure (pure t, a))


------------------------------------------------------------------------------
renew :: (Eq k, Hashable k)
    => NominalDiffTime
    -> UTCTime -> k -> TimeoutMap k a
    -> (TimeoutMap k a, Maybe NominalDiffTime)
renew = adjust . (,) . pure


------------------------------------------------------------------------------
adjust :: (Eq k, Hashable k)
    => (a -> (Maybe NominalDiffTime, a))
    -> UTCTime -> k -> TimeoutMap k a
    -> (TimeoutMap k a, Maybe NominalDiffTime)
adjust = update . (pure .)


------------------------------------------------------------------------------
update :: (Eq k, Hashable k)
    => (a -> Maybe (Maybe NominalDiffTime, a))
    -> UTCTime -> k -> TimeoutMap k a
    -> (TimeoutMap k a, Maybe NominalDiffTime)
update = mutate empty


------------------------------------------------------------------------------
delete :: (Eq k, Hashable k)
    => UTCTime -> k -> TimeoutMap k a -> TimeoutMap k a
delete = fst .:. mutate empty (const empty)


------------------------------------------------------------------------------
mutate :: (Eq k, Hashable k)
    => Maybe (NominalDiffTime, a)
    -> (a -> Maybe (Maybe NominalDiffTime, a))
    -> UTCTime -> k -> TimeoutMap k a
    -> (TimeoutMap k a, Maybe NominalDiffTime)
mutate inserter updater = fmap fst .:. mutateWith inserter ((,) () . updater)


------------------------------------------------------------------------------
mutateWith :: (Eq k, Hashable k)
    => Maybe (NominalDiffTime, a)
    -> (a -> (b, Maybe (Maybe NominalDiffTime, a)))
    -> UTCTime -> k -> TimeoutMap k a
    -> (TimeoutMap k a, (Maybe NominalDiffTime, Maybe b))
mutateWith inserter updater now = (swap .) . iso . H.alterF go
  where
    go (Just (then_, a)) | then_ >= now = case updater a of
        (b, Nothing) -> ((empty, pure b), empty)
        (b, Just (mtimeout, a')) -> ((pure timeout, pure b), pure (then', a'))
          where
            timeout = maybe id max mtimeout diff
            then' = addUTCTime timeout now
      where
        diff = diffUTCTime then_ now
    go _ = case inserter of
        Nothing -> ((empty, empty), empty)
        Just (timeout, a) -> ((pure timeout, empty), pure (then_, a))
          where
            then_ = addUTCTime timeout now


------------------------------------------------------------------------------
clean :: UTCTime -> TimeoutMap k a -> TimeoutMap k a
clean = (runIdentity .) . iso . (Identity .) . H.filter . (. fst) . (<=)


------------------------------------------------------------------------------
iso :: Functor f => (HashMap k (UTCTime, a) -> f (HashMap l (UTCTime, b)))
    -> TimeoutMap k a -> f (TimeoutMap l b)
iso f (TimeoutMap m) = TimeoutMap <$> f m


------------------------------------------------------------------------------
(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:.) f g a b c = f (g a b c)
infixr 8 .:.
