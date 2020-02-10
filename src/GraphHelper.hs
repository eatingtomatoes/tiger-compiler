module GraphHelper where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace
import Data.List (groupBy, sort)
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
