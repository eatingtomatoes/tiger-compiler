{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Utility.DirectedGraph.MapBasedDirectedGraph
  ( MapBasedDirectedGraph
  , module Utility.DirectedGraph
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Lens (makeLenses, over, view, views, _2)
import Data.Maybe (fromMaybe)

import Utility.DirectedGraph
import Utility.Conversion
 
data MapBasedDirectedGraphNode v d
  = MapBasedDirectedGraphNode
  { _mdgnData :: d
  , _mdgnPrevs :: Set.Set v
  , _mdgnSuccs :: Set.Set v
  }

newtype MapBasedDirectedGraph v d
  = MapBasedDirectedGraph
  { _mdgMap :: Map.Map v (MapBasedDirectedGraphNode v d)
  }

makeLenses ''MapBasedDirectedGraphNode
makeLenses ''MapBasedDirectedGraph

getNode
  :: Ord v
  => v
  -> MapBasedDirectedGraph v d
  -> Maybe (MapBasedDirectedGraphNode v d)
getNode x = views mdgMap (Map.lookup x)

putNode
  :: Ord v
  => v
  -> MapBasedDirectedGraphNode v d
  -> MapBasedDirectedGraph v d
  -> MapBasedDirectedGraph v d
putNode x n = over mdgMap (Map.insert x n)

modifyNode
  :: Ord v
  => v
  -> (MapBasedDirectedGraphNode v d -> MapBasedDirectedGraphNode v d)
  -> MapBasedDirectedGraph v d
  -> MapBasedDirectedGraph v d
modifyNode x f g = fromMaybe g $ do
  xn <- getNode x g
  return $ putNode x (f xn) g
 
instance Ord v => DirectedGraph (MapBasedDirectedGraph v d) where
  type VertexId (MapBasedDirectedGraph v d) = v
  type VertexData (MapBasedDirectedGraph v d) = d

  empty = MapBasedDirectedGraph mempty

  connectTo x y
    = modifyNode x (over mdgnSuccs (Set.insert y))
    . modifyNode y (over mdgnPrevs (Set.insert x))

  disconnectTo x y
    = modifyNode x (over mdgnSuccs (Set.delete y))
    . modifyNode y (over mdgnPrevs (Set.delete x))
  doesConnectTo x y = maybe False (views mdgnSuccs (Set.member y)) . getNode x


  existVertex x = views mdgMap (Map.member x)

  addVertex x d = putNode x MapBasedDirectedGraphNode
    { _mdgnData = d
    , _mdgnPrevs = mempty
    , _mdgnSuccs = mempty
    }

  removeVertex x g = fromMaybe g $ do
    xn <- getNode x g
    return $ foldr h (foldr f g $ _mdgnPrevs xn) (_mdgnSuccs xn)
    where
       f y = modifyNode y (over mdgnSuccs $ Set.delete x)
       h y = modifyNode y (over mdgnPrevs $ Set.delete x)

  dataOf x = fmap (view mdgnData) . getNode x

  succOf x = fmap (from . _mdgnSuccs) . getNode x

  -- succOf x = fmap Set.toList . succSetOf x

  -- succSetOf x = fmap _mdgnSuccs . getNode x

  predOf x = fmap Set.toList . predSetOf x

  predSetOf x = fmap _mdgnPrevs . getNode x

  vertexOf = views mdgMap Map.keys

  vertexSetOf = views mdgMap Map.keysSet
 
  flattenData = Map.toList . Map.map _mdgnData . _mdgMap

  flattenSucc = fmap (over _2 Set.toList) . flattenSuccSet

  flattenSuccSet = Map.toList . Map.map _mdgnSuccs . _mdgMap

  flattenPred = fmap (over _2 Set.toList) . flattenPredSet
  
  flattenPredSet = Map.toList . Map.map _mdgnPrevs . _mdgMap

