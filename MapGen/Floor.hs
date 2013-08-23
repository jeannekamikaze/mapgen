module MapGen.Floor
(
    Floor
,   Corridor
,   Room
    -- * Construction
,   newFloor
,   randFloor
,   mkFloor
    -- * Operations
,   zipFloors
    -- * Access
,   corridors
,   rooms
)
where

import MapGen.KdTree
import MapGen.Space

import Control.Applicative
import Control.Monad.Random
import Data.List (zipWith)
import Data.Foldable as Fold
import Data.Map as Map
import Data.Monoid
import Data.Traversable
import System.Random
import Prelude as P hiding (zipWith)

type Corridor a = (Line,a)
type Room a = (Quad,a)

data Floor a = Floor
     { corridor :: Map Line a
     , room     :: Map Quad a
     } deriving Show

instance Functor Floor where
         fmap f decor = Floor corridor' room'
              where corridor' = fmap f $ corridor decor
                    room' = fmap f $ room decor

instance Monoid (Floor a) where
         mempty = Floor Map.empty Map.empty

         mappend f1 f2 = Floor corridor' room'
                 where corridor' = mappend (corridor f1) (corridor f2)
                       room' = mappend  (room f1) (room f2)

instance Foldable Floor where
         foldr f a (Floor corridor room) = Fold.foldr f (Fold.foldr f a corridor) room

instance Traversable Floor where
         traverse f floor =
                  Floor <$> traverse f (corridor floor) <*> traverse f (room floor)

newFloor :: KdTree -> (Line -> a) -> (Quad -> a) -> Floor a
newFloor kdt l q = Floor (set l splitters) (set q quads)
         where set f xs' = let xs = xs' kdt
                           in Map.fromList $ zipWith (,) xs (fmap f xs)

randFloor :: KdTree -> (Line -> Rand a) -> (Quad -> Rand a) -> Rand (Floor a)
randFloor kdt randLine randQuad =
          let r f xs' = let xs = xs' kdt
                        in fmap (Map.fromList . (zipWith (,) xs)) $ P.mapM f xs
          in do
             corridors <- r randLine splitters
             rooms <- r randQuad quads
             return $ Floor corridors rooms

mkFloor :: KdTree -> (Line -> Rand a) -> (Quad -> Rand a) -> Seed -> Floor a
mkFloor kdt l q s = evalRand (randFloor kdt l q) $ mkStdGen s

zipFloors :: (a -> b -> c) -> Floor a -> Floor b -> Floor c
zipFloors f f1 f2 = Floor corridor' room'
          where corridor' = Map.intersectionWith f (corridor f1) (corridor f2)
                room' = Map.intersectionWith f (room f1) (room f2)

corridors :: Floor a -> [Corridor a]
corridors = assocs . corridor

rooms :: Floor a -> [Room a]
rooms = assocs . room