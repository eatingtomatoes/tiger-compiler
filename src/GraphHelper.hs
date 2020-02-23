{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module GraphHelper where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace
import Data.List (groupBy, sort, intersperse)
import Data.Tuple (swap)

import Graph

toGraphviz :: (Show a, Ord a) => String -> Graph a -> String
toGraphviz title graph = "graph " <> title <> "{\n" <> nodes <> edges <> "}\n"
-- toGraphviz title graph = "graph " <> title <> "{\n" <> edges <> "}\n"
  where
    connections = [ (x, n) | (x, ns) <- Map.toList graph, n <- Set.toList ns, x < n ]
    edges = concatMap f connections
      where
        f (x, n) = show x <> " -- " <> show n <> ";\n"
    nodes = concatMap f $ Map.toList sizeMap
      where
        f (x, cnt) = show x <> " [label = \"" <> show x <> "["<> show cnt <> "]\"];\n"
      
    sizeMap = Map.unionsWith (+) . fmap count $ [connections, fmap swap connections] 
    count = Map.fromList . fmap g . groupBy f . sort 
      where
        f x y = fst x == fst y
        g xs = (fst $ head xs, length xs)

traceGraph :: (Show a, Ord a) => String -> Graph a -> Graph a
traceGraph title graph = trace (toGraphviz title graph) graph

traceGraph' :: (Show a, Ord a) => String -> Graph a -> b -> b
traceGraph' title graph = trace (toGraphviz title graph) 

toDirectedGraphviz :: (Show a, Ord a) => String -> Graph a -> String
toDirectedGraphviz title graph = "digraph " <> title <> "{\n" <> verticesRep <> ";\n" <> edges <> "}\n"
  where
    connections = [ (x, n) | (x, ns) <- Map.toList graph, n <- Set.toList ns ]
    verticesRep = concat $ intersperse ";\n" $ fmap show $ nodesOf graph
    edges = concatMap f connections
      where
        f (x, n) = show x <> " -> " <> show n <> ";\n"

-- toDirectedGraphvizWithFun :: Ord a => String -> (a -> Show) -> Graph a -> String
-- toDirectedGraphvizWithFun title graph = "digraph " <> title <> "{\n" <> verticesRep <> ";\n" <> edges <> "}\n"
--   where
--     connections = [ (x, n) | (x, ns) <- Map.toList graph, n <- Set.toList ns ]
--     verticesRep = concat $ intersperse ";\n" $ fmap show $ nodesOf graph
--     edges = concatMap f connections
--       where
--         f (x, n) = show x <> " -> " <> show n <> ";\n"


traceDirectedGraph :: (Show v, Ord v) => String -> Graph v -> Graph v
traceDirectedGraph title graph = trace (toDirectedGraphviz title graph) graph

traceDirectedGraph' :: (Show a, Ord a) => String -> Graph a -> b -> b
traceDirectedGraph' title graph = trace (toDirectedGraphviz title graph) 

class DrawableDirectedGraph g where
  type Vertex g
  successors :: Vertex g -> g -> Maybe [(Vertex g)]
  vertices :: g -> [(Vertex g)]

toDirectedGraphviz_
  :: (Show (Vertex g), DrawableDirectedGraph g)
  => String -> g -> String
toDirectedGraphviz_ title graph = "digraph " <> title <> "{\n" <> verticesRep <> ";\n" <> edgesRep <> "}\n"
  where
    connections = [ (x, n) | x <- vertices graph, n <- maybe mempty id $ successors x graph]
    verticesRep = concat $ intersperse ";\n" $ fmap show $ vertices graph
    edgesRep = concatMap f connections
      where
        f (x, n) = show x <> " -> " <> show n <> ";\n"

traceDirectedGraph_ :: (Show (Vertex g), DrawableDirectedGraph g) => String -> g -> g
traceDirectedGraph_ title graph = trace (toDirectedGraphviz_ title graph) graph

traceDirectedGraph_' :: (Show (Vertex g), DrawableDirectedGraph g) => String -> g -> b -> b
traceDirectedGraph_' title graph = trace (toDirectedGraphviz_ title graph) 
