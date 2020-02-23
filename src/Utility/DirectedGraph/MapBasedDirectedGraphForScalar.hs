{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Utility.DirectedGraph.MapBasedDirectedGraphForScalar
  ( MapBasedDirectedGraphForScalar
  , module Utility.DirectedGraph
  ) where

import Control.Lens (makeLenses, over, views)

import Utility.DirectedGraph
import Utility.Visualize
import qualified Utility.DirectedGraph.MapBasedDirectedGraph as MDG
 
newtype MapBasedDirectedGraphForScalar v
  = MapBasedDirectedGraphForScalar
  { _mdgiMap :: MDG.MapBasedDirectedGraph v ()
  } 

makeLenses ''MapBasedDirectedGraphForScalar

ensure :: Ord v => v -> MDG.MapBasedDirectedGraph v () -> MDG.MapBasedDirectedGraph v ()
ensure x g
  | existVertex x g = g
  | otherwise = addVertex x () g

ensureAll :: Ord v => [v] -> MDG.MapBasedDirectedGraph v () -> MDG.MapBasedDirectedGraph v ()
ensureAll = flip (foldr ensure) 

instance Ord v => DirectedGraph (MapBasedDirectedGraphForScalar v) where 
  type VertexId (MapBasedDirectedGraphForScalar v) = v
  type VertexData (MapBasedDirectedGraphForScalar v) = ()

  empty = MapBasedDirectedGraphForScalar empty

  connectTo x y = over mdgiMap $ connectTo x y . ensureAll [x, y]

  disconnectTo x y = over mdgiMap $ disconnectTo x y
 
  doesConnectTo x y = views mdgiMap $ doesConnectTo x y
 
  existVertex x = views mdgiMap $ existVertex x

  addVertex x _ = over mdgiMap $ addVertex x ()

  removeVertex x = over mdgiMap $ removeVertex x 

  dataOf _ _ = Just ()

  succOf x = views mdgiMap $ succOf x
  
  -- succOf x = views mdgiMap $ succOf x

  -- succSetOf x = views mdgiMap $ succSetOf x
  
  predOf x = views mdgiMap $ predOf x

  predSetOf x = views mdgiMap $ predSetOf x

  vertexOf = views mdgiMap vertexOf

  vertexSetOf = views mdgiMap vertexSetOf

  flattenData = flattenData . _mdgiMap

  flattenSucc = flattenSucc . _mdgiMap

  flattenSuccSet = flattenSuccSet . _mdgiMap

  flattenPred = flattenPred . _mdgiMap
  
  flattenPredSet = flattenPredSet . _mdgiMap


instance (Ord v, Show v) => DrawableDirectedGraph (MapBasedDirectedGraphForScalar v) where
  showVertexId v _ = show v
  showVertexData _ _ = ""
