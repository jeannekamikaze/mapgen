module Floor
(
    Decoration(..)
,   Door
,   randRoom
,   randCorridor
)
where

import qualified MapGen.Floor as F
import MapGen.KdTree
import MapGen.Space

import Control.Monad.Random
import Data.List (intersect)
import System.Random

type Door = Vec2

data Side = L | R | T | B deriving Eq

data Decoration
     = Room
     { doors :: [Door]
     }
     | Corridor
     {
     }
     deriving Show

randRoom :: Width -> Height -> Quad -> Rand Decoration
randRoom w h q = do
         nDoors <- oneOf [1,2]
         doors <- sequence (replicate nDoors $ randDoor w h q)
         return $ Room doors

randDoor :: Width -> Height -> Quad -> Rand Door
randDoor w h q@(Quad p1 p2) = do
         s <- randSide w h q
         f <- inRange (0,1) :: Rand Double
         let x' = lerp f (fromIntegral $ x p1) (fromIntegral $ x p2)
             y' = lerp f (fromIntegral $ y p1) (fromIntegral $ y p2)
             lerp a x y = floor $ x + a*(y-x)
         return $ case s of
                L -> Vec2 (x p1) y'
                R -> Vec2 (x p2) y'
                T -> Vec2 x' (y p2)
                B -> Vec2 x' (y p1)

randSide :: Width -> Height -> Quad -> Rand Side
randSide w h (Quad p1 p2) =
         let w' = w-1; h' = h-1
             s1 = if x p1 == 0  || x p2 == 0  then [R, T, B] else ss
             s2 = if x p1 == w' || x p2 == w' then [L, T, B] else ss
             s3 = if y p1 == 0  || y p2 == 0  then [L, R, T] else ss
             s4 = if y p1 == h' || y p2 == h' then [L, R, B] else ss
             ss = [L, R, T, B]
         in oneOf (intersect s1 $ intersect s2 $ intersect s3 s4)

randCorridor :: Line -> Rand Decoration
randCorridor _ = return Corridor