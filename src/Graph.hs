module Graph
  ( Graph
  , connectTo
  , connect
  , connectGroup
  , disconnect
  , disconnectFrom
  , deleteNode
  , neighborsOf
  , neighborsNumOf
  , nodesOf
  , isNeighborOf
  , isNeighbors
  , mergeTo
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (tails)
import Data.Foldable (toList)

type Graph a = Map.Map a (Set.Set a)

connectTo :: Ord a => a -> a -> Graph a -> Graph a
connectTo x y = case compare x y of
  EQ -> id
  _ -> Map.alter (Just . maybe (Set.singleton y) (Set.insert y)) x

connect :: Ord a => a -> a -> Graph a -> Graph a
connect x y = connectTo x y . connectTo y x 

connectGroup :: (Foldable f, Ord a) => f a -> Graph a -> Graph a 
connectGroup group = f $ toList group
  where
    f xs g = foldr h g $ zip xs $ tail $ tails xs
    h (x, xs) g = foldr (connect x) g xs
    
disconnect :: Ord a => a -> a -> Graph a -> Graph a
disconnect x y = disconnectFrom x y . disconnectFrom y x

disconnectFrom :: Ord a => a -> a -> Graph a -> Graph a
disconnectFrom x y = Map.alter (fmap $ Set.delete y) x

deleteNode :: Ord a => a -> Graph a -> Graph a
deleteNode x g = maybe g (foldr (flip disconnectFrom x) $ Map.delete x g) $ Map.lookup x g

neighborsOf :: Ord a => a -> Graph a -> Maybe (Set.Set a)
neighborsOf x g = Map.lookup x g 

neighborsNumOf :: Ord a => a -> Graph a -> Maybe Int
neighborsNumOf x = fmap Set.size . neighborsOf x

nodesOf :: Ord a => Graph a -> [a]
nodesOf = Map.keys

isNeighborOf :: Ord a => a -> a -> Graph a -> Bool
isNeighborOf x y g = maybe False (Set.member x) $ neighborsOf y g

isNeighbors :: Ord a => a -> a -> Graph a -> Bool
isNeighbors x y g = isNeighborOf x y g || isNeighborOf y x g

-- Add edges between x's neighbors and y then delete x
mergeTo :: Ord a => a -> a -> Graph a -> Graph a
mergeTo x y g = deleteNode x . foldr f g $ maybe mempty id $ neighborsOf x g 
  where
    f a = connect a y . disconnect a x 

                    
