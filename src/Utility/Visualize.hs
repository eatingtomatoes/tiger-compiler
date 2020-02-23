{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Utility.Visualize
  ( DrawableDirectedGraph
  , showVertexId
  , showVertexData
  , visualizeDirectedGraph
  , DrawableGraph
  , visualizeGraph
  ) where

import Data.List (intersperse)
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Text.Printf

import Utility.DirectedGraph
import Utility.Graph

class DirectedGraph g => DrawableDirectedGraph g where
  showVertexId :: VertexId g -> g -> String
  showVertexData :: VertexData g -> g -> String

  visualizeDirectedGraph :: String -> g -> String
  visualizeDirectedGraph title g = "digraph " <> title <> " {\n" <> verticesRep <> ";\n" <> edgesRep <> "}\n"
    where
      edges = [ (x, n) | x <- vertexOf g, n <- maybe mempty id $ succOf x g]
      verticesRep = concat $ intersperse ";\n" $ fmap (flip showVertexId g) $ vertexOf g
      edgesRep = concatMap f edges
        where
          f (x, y) = showVertexId x g <> " -> " <> showVertexId y g <> ";\n"

class (DrawableDirectedGraph g, Graph g, Ord (VertexId g)) => DrawableGraph g where
  visualizeGraph :: String -> g -> String
  visualizeGraph title g = "graph " <> title <> " {\n" <> verticesRep <> ";\n" <> edgesRep <> "}\n"
    where
      connections = [ (x, y) | x <- vertexOf g, y <- maybe mempty id $ neighborOf x g, x < y ]
      edgesRep = concatMap f connections
        where
          f (x, n) = showVertexId x g <> " -- " <> showVertexId n g <> ";\n"
      verticesRep = concatMap f $ catMaybes $ fmap (\v -> fmap (v, ) $ fmap Set.size $ neighborSetOf v g) $ vertexOf g
        where
          f (x, cnt) = printf "%s [label=\"[%d]\"];\n" (showVertexId x g) cnt
