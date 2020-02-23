module MiddleEnd.Quadruple.Loop.LoopInvariant
  ( calcLoopInvariant
  ) where

import qualified Data.Set as Set
import Safe (atMay)
import Debug.Pretty.Simple

import MiddleEnd.Quadruple
import qualified MiddleEnd.Quadruple.ReachingDefinition as RD

import Utility.Fixpoint

calcLoopInvariant :: [Quadruple] -> (Int, [Int])-> [Int]
calcLoopInvariant quads (entry, exits) = Set.toList $ fixpoint (==) calc mempty
  where
    inLoop i = entry <= i && any (i <=) exits

    calc :: Set.Set Int -> Set.Set Int
    calc invariants
      = id
      $ Set.union invariants
      $ Set.fromList
      $ fmap firstOne
      $ filter f
      $ zip3 [0..] quads
      $ fst
      $ RD.calcInAndOut quads
      where
        f :: (Int, Quadruple, Set.Set Int) -> Bool
        f (i, q, rds) = inLoop i && (not $ Set.member i invariants) && case q of
          BinQ _ _ v1 v2 -> safe v1 && safe v2
          UnQ _ _ v1 -> safe v1
          MoveQ _ v1 -> safe v1 
          _ -> False
          where
            safe var = case var of
              TempV t -> case defs t of
                [] -> True
                [x] -> Set.member x invariants
                _ -> False
              _ -> True
              where
                defs t = Set.toList
                  $ Set.filter (\d -> maybe False (== t) $ quads `atMay` d >>= RD.destOf)
                  $ Set.filter inLoop rds
        firstOne (x, _, _) = x
