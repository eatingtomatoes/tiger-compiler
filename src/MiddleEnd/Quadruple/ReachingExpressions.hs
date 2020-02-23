{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module MiddleEnd.Quadruple.ReachingExpressions
  ( calcIn
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Maybe (mapMaybe)
import Safe (atMay)
import Control.Arrow

import Translation.Temp
import MiddleEnd.Quadruple
import MiddleEnd.Quadruple.AvailableExpressions as AE
import MiddleEnd.Quadruple.ReachingDefinition as RD

calcIn :: [Quadruple] -> [Map.Map Expression Temp]
calcIn quads = zipWith f aeIns rdIns
  where
    aeIns  = fst $ AE.calcInAndOut quads
    rdIns = fst $ RD.calcInAndOut quads
    f :: Set.Set Expression -> Set.Set Int -> Map.Map Expression Temp
    f es rds = Map.fromList $ mapMaybe g $ Set.toList es
      where
        g :: Expression -> Maybe (Expression, Temp)
        g expr = fmap (expr, )
          $ flip atMay 0
          $ fmap fst
          $ filter ((== expr) . snd)
          $ mapMaybe (atMay quads >=> \q -> (,) <$> RD.destOf q <*> AE.exprOf q)
          $ Set.toList rds
