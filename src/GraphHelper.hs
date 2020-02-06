module GraphHelper where

import Control.Monad
import Control.Monad.Writer
import Control.Arrow
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (groupBy, sort)
import Data.Tuple (swap)


import Graph

toGraphviz :: (Show a, Ord a) => String -> Graph a -> String
toGraphviz title graph = "graph " <> title <> "{\n" <> nodes <> edges <> "}\n"
  where
    connections = [ (x, n) | (x, ns) <- Map.toList graph, n <- Set.toList ns, x <= n ]
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

dumpGraph :: (Show a, Ord a) => String -> Graph a -> IO ()
dumpGraph title graph = do
  writeFile (title <> ".dot") $ toGraphviz title graph
