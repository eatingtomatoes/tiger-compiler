{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Utility.Bucket
  ( Bucket
  , insert
  , assocOf
  , isAssocWith  
  , fromMap
  , toList
  , reverse
  ) where

import Prelude hiding (reverse)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype Bucket v d = Bucket { unBucket :: Map.Map v (Set.Set d) }

instance (Ord v, Ord d) => Semigroup (Bucket v d) where
  xs <> ys = Bucket $ Map.unionWith Set.union (unBucket xs) (unBucket ys)

instance Semigroup (Bucket v d) => Monoid (Bucket v d) where
  mempty = Bucket Map.empty

insert :: (Ord v, Ord d) => v -> d -> Bucket v d -> Bucket v d
insert v d = Bucket . Map.alter (Just . maybe (Set.singleton d) (Set.insert d)) v . unBucket

assocOf :: (Ord v, Ord d) => v -> Bucket v d -> Maybe (Set.Set d)
assocOf v = Map.lookup v . unBucket

isAssocWith :: (Ord v, Ord d) => d -> v -> Bucket v d -> Maybe Bool
isAssocWith d v = maybe Nothing (Just . Set.member d) . assocOf v

fromMap :: Map.Map v (Set.Set d) -> Bucket v d
fromMap m = Bucket m

toList :: Bucket v d -> [(v, Set.Set d)]
toList = Map.toList . unBucket

reverse :: (Ord v, Ord d) => Bucket v d -> Bucket d v
reverse = Map.foldrWithKey f mempty . unBucket
  where
    f v ds bucket = foldr (flip insert v) bucket ds
