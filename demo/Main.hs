{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Floor
import IO

import MapGen.Floor
import MapGen.KdTree hiding (mapM_)
import MapGen.IO
import MapGen.Space

import Data.Foldable as F (mapM_)
import System.Console.CmdArgs
import System.Random
import Prelude as P

data MapGen = MapGen
     { file       :: FilePath
     , format     :: ImageFormat
     , width      :: Width
     , height     :: Height
     , depth      :: Depth
     , splitStdev :: Stdev
     , seed       :: Seed
     } deriving (Data, Typeable, Show)

defStdev = defaultSplitStdev

defaultArgs = cmdArgsMode $ MapGen
            { file       = "map"     &= name "f" &= help "Output file"
            , format     = PNG       &= name "t" &= help "Output file format."
                                     &= typ "SVG | PNG"
            , width      = 64        &= name "w" &= help "Map width"
            , height     = 64        &= name "h" &= help "Map height"
            , depth      = 4         &= name "d" &= help "Maximum kd-tree depth"
            , splitStdev = defStdev  &= name "p" &= help "Split standard deviation"
            , seed       = (-1)      &= name "s" &= help "Random seed"
            }

main = do
     (MapGen file format w h d stdev s') <- cmdArgsRun defaultArgs
     s <- if s' == (-1) then randomIO else return s'
     putStrLn $ "Map size: " ++ show w ++ "x" ++ show h
     putStrLn $ "Depth: " ++ show d
     putStrLn $ "Split stdev: " ++ show stdev
     putStrLn $ "Seed: " ++ show s
     let kdt = kdtree w h d (Just stdev) s
     putSplitters kdt
     putQuads kdt
     let floor = mkFloor kdt randCorridor (randRoom w h) s
     F.mapM_ (putStrLn . show) floor
     toFile file format kdt w h [F.mapM_ renderDecor floor]

putSplitters = P.mapM_ (putStrLn . show) . splitters
putQuads = P.mapM_ (putStrLn . show) . quads