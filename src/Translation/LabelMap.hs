{-# LANGUAGE TupleSections #-}

module Translation.LabelMap
  ( LabelMap
  , buildLabelMap
  , posOf
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Translation.Label

newtype LabelMap = LabelMap { unLabelMap :: Map.Map Label Int }
  
buildLabelMap :: (a -> Maybe Label) -> [a] -> LabelMap
buildLabelMap labelOf
  = LabelMap
  . Map.fromList
  . catMaybes
  . zipWith (\pos -> fmap (, pos)) [0..]
  . fmap labelOf

posOf :: Label -> LabelMap -> Maybe Int
posOf l = Map.lookup l . unLabelMap 
