{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Translation.SSAToQuadruple
  (
  ) where


import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Pretty.Simple
import Data.Maybe (catMaybes)
import Data.Foldable
import Control.Monad.Loops
import Control.Monad.State
import Safe (headMay)
import Control.Lens (makeLenses, modifying, over,  _2, view)
import Data.List (sortOn)
import Text.Printf 
  
