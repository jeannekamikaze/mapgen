{-# LANGUAGE DeriveDataTypeable #-}
module MapGen.IO
(
    ImageFormat(..)
,   toFile
,   renderKdTree
)
where

import MapGen.KdTree as KdTree
import MapGen.Space

import Data.Data
import Control.Monad.ST
import Graphics.Rendering.Cairo

data ImageFormat = SVG | PNG deriving (Data, Typeable, Show)

toFile :: FilePath -> ImageFormat -> KdTree -> Width -> Height ->  IO ()
toFile file' format kdt w h =
       let w' = fromIntegral w
           h' = fromIntegral h
           file = file' ++ case format of SVG -> ".svg"; PNG -> ".png"
       in case format of
          SVG -> withSVGSurface file (w'*0.8) (h'*0.8) $ flip renderWith $
              render w' h' kdt
          PNG -> withImageSurface FormatRGB24 w h $ \surface -> do
              renderWith surface $ render w' h' kdt
              surfaceWriteToPNG surface file

render :: Double -> Double -> KdTree -> Render ()
render w h kdt = do
       -- Background
       rectangle 0 0 w h
       setSourceRGB 0.5 0.5 0.5
       fill
       -- Line configuration
       setSourceRGB 1 1 1
       setLineWidth 1;
       -- Render kd-tree
       renderKdTree kdt

renderKdTree :: KdTree -> Render ()
renderKdTree = KdTree.mapM_ r
             where r (Leaf _) = return ()
                   r (Node l c1 c2) =
                     let (x1,y1,x2,y2) = coords l
                     in do moveTo x1 y1; lineTo x2 y2; stroke

coords :: Line -> (Double, Double, Double, Double)
coords (Line (Vec2 x1 y1) (Vec2 x2 y2)) =
       (fromIntegral x1+0.5, fromIntegral y1+0.5
       ,fromIntegral x2+0.5, fromIntegral y2+0.5)