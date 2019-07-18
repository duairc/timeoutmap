{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Data.TimeoutMap
    ( TimeoutMap (TimeoutMap), toList
    , lookup
    , insert, renew
    , adjust, update
    , delete
    , clean
    )
where

-- aeson ---------------------------------------------------------------------
import           Data.Aeson
                     ( FromJSON, ToJSON, ToJSONKey, toEncoding
                     , genericToEncoding, defaultOptions
                     )


-- base ----------------------------------------------------------------------
import           Control.Applicative (empty)
import           Data.Bifunctor (bimap, first, second)
import           Data.Maybe (fromJust, mapMaybe)
import           Data.Semigroup (Semigroup)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Prelude hiding (lookup)


-- deepseq -------------------------------------------------------------------
import           Control.DeepSeq (NFData)


-- hashable ------------------------------------------------------------------
import           Data.Hashable (Hashable)


-- hashable-time -------------------------------------------------------------
import           Data.Hashable.Time ()


-- time ----------------------------------------------------------------------
import           Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)


-- unordered-containers ------------------------------------------------------
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H


------------------------------------------------------------------------------
newtype TimeoutMap k a = TimeoutMap (HashMap k (a, UTCTime))
  deriving
    ( Eq, Ord, Read, Show, Generic, Typeable
    , NFData, Hashable, FromJSON
    , Functor
    )


------------------------------------------------------------------------------
instance (ToJSONKey k, ToJSON a) => ToJSON (TimeoutMap k a) where
    toEncoding = genericToEncoding defaultOptions


------------------------------------------------------------------------------
instance (Eq k, Hashable k, Semigroup a) => Semigroup (TimeoutMap k a) where
    TimeoutMap x <> TimeoutMap y = TimeoutMap $ H.unionWith go x y
      where
        go (a, s) (b, t) = (a <> b, max s t)


------------------------------------------------------------------------------
instance (Eq k, Hashable k, Semigroup a) => Monoid (TimeoutMap k a) where
    mempty = TimeoutMap mempty


------------------------------------------------------------------------------
toList :: UTCTime -> TimeoutMap k a -> [(k, a)]
toList now (TimeoutMap as) = mapMaybe go $ H.toList as
  where
    go (k, (a, t)) | t >= now = pure (k, a)
    go _ = empty


------------------------------------------------------------------------------
lookup :: (Eq k, Hashable k)
    => k -> UTCTime -> TimeoutMap k a -> (TimeoutMap k a, Maybe a)
lookup = fmap fst ... mutateWith Nothing (\a -> (Just (a, Nothing), a))


------------------------------------------------------------------------------
insert :: (Eq k, Hashable k)
    => a -> NominalDiffTime
    -> k -> UTCTime -> TimeoutMap k a -> (TimeoutMap k a, UTCTime)
insert a t = fmap fromJust ... mutate (Just (a, t)) (\_ -> Just (a, Just t))


------------------------------------------------------------------------------
renew :: (Eq k, Hashable k)
    => NominalDiffTime
    -> k -> UTCTime -> TimeoutMap k a -> (TimeoutMap k a, Maybe UTCTime)
renew = adjust . flip (,) . Just


------------------------------------------------------------------------------
adjust :: (Eq k, Hashable k)
    => (a -> (a, Maybe NominalDiffTime))
    -> k -> UTCTime -> TimeoutMap k a -> (TimeoutMap k a, Maybe UTCTime)
adjust = update . (Just .)


------------------------------------------------------------------------------
update :: (Eq k, Hashable k)
    => (a -> Maybe (a, Maybe NominalDiffTime))
    -> k -> UTCTime -> TimeoutMap k a -> (TimeoutMap k a, Maybe UTCTime)
update = mutate Nothing


------------------------------------------------------------------------------
delete :: (Eq k, Hashable k)
    => k -> UTCTime -> TimeoutMap k a -> TimeoutMap k a
delete = fst ... mutate Nothing (const Nothing)


------------------------------------------------------------------------------
mutate :: (Eq k, Hashable k)
    => Maybe (a, NominalDiffTime)
    -> (a -> Maybe (a, Maybe NominalDiffTime))
    -> k -> UTCTime -> TimeoutMap k a -> (TimeoutMap k a, Maybe UTCTime)
mutate inserter updater = fmap snd ... mutateWith inserter ((, ()) . updater)


------------------------------------------------------------------------------
mutateWith :: (Eq k, Hashable k)
    => Maybe (a, NominalDiffTime)
    -> (a -> (Maybe (a, Maybe NominalDiffTime), b))
    -> k -> UTCTime -> TimeoutMap k a
    -> (TimeoutMap k a, (Maybe b, Maybe UTCTime))
mutateWith inserter updater key now = tmap (alterWith (expiry . go) key)
  where
    go (Just (a, then_)) | then_ >= now = bimap time Just (updater a)
      where
        time = fmap (second (maybe then_ (max then_ . flip addUTCTime now)))
    go _ = (second (flip addUTCTime now) <$> inserter, Nothing)
    expiry (mutation, result) = (mutation, (result, snd <$> mutation))


------------------------------------------------------------------------------
clean :: UTCTime -> TimeoutMap k a -> TimeoutMap k a
clean = tmap_ . H.filter . (. snd) . (<=)


------------------------------------------------------------------------------
tmap :: (HashMap k (a, UTCTime) -> (HashMap k (a, UTCTime), b))
    -> TimeoutMap k a -> (TimeoutMap k a, b)
tmap f (TimeoutMap m) = first TimeoutMap $ f m


------------------------------------------------------------------------------
tmap_ :: (HashMap k (a, UTCTime) -> HashMap k (a, UTCTime))
    -> TimeoutMap k a -> TimeoutMap k a
tmap_ f (TimeoutMap m) = TimeoutMap $ f m


------------------------------------------------------------------------------
alterWith :: (Eq k, Hashable k)
    => (Maybe a -> (Maybe a, b)) -> k -> HashMap k a -> (HashMap k a, b)
alterWith f k m = first (maybe delete_ insert_) $ f lookup_
  where
    lookup_ = H.lookup k m
    delete_ = H.delete k m
    insert_ v = H.insert k v m
{-# INLINE alterWith #-}


------------------------------------------------------------------------------
(...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(...) f g a b c = f (g a b c)
infixr 9 ...
