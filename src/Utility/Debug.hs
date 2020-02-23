module Utility.Debug
  ( traceDirectedGraph
  , traceDirectedGraphId
  , pTraceWith
  , quote
  ) where

import Utility.Visualize
import Debug.Trace (trace)
import Debug.Pretty.Simple

traceDirectedGraph :: DrawableDirectedGraph g =>  String -> g -> x -> x
traceDirectedGraph title g x = trace (visualizeDirectedGraph title g) x

traceDirectedGraphId :: DrawableDirectedGraph g =>  String -> g -> g
traceDirectedGraphId title g = traceDirectedGraph title g g

pTraceWith :: (a -> String) -> a -> a
pTraceWith f x = pTrace (f x) x

quote :: Show a => a -> String
quote x = '`' : (show x <> "'")
