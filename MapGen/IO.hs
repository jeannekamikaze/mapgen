{-# LANGUAGE DeriveDataTypeable #-}
module MapGen.IO
(
    ImageFormat(..)
,   toFile
,   renderKdTree
)
where

import MapGen.Floor
import MapGen.KdTree as KdTree
import MapGen.Space

import Data.Data
import Control.Monad.ST
import Graphics.Rendering.Cairo
import Prelude as P

data ImageFormat = SVG | PNG deriving (Data, Typeable, Show)

toFile :: FilePath -> ImageFormat -> KdTree -> Width -> Height
       -> [Render ()]
       -> IO ()
toFile file' format kdt w h renderDecorations =
       let w' = fromIntegral w
           h' = fromIntegral h
           file = file' ++ case format of SVG -> ".svg"; PNG -> ".png"
           renderImage = do
                       render w' h' kdt
                       sequence_ renderDecorations
       in case format of
          SVG -> withSVGSurface file (w'*0.8) (h'*0.8) $ flip renderWith $ renderImage
          PNG -> withImageSurface FormatRGB24 w h $ \surface -> do
              renderWith surface renderImage
              surfaceWriteToPNG surface file

render :: Double -> Double -> KdTree -> Render ()
render w h kdt = do
       -- Background
       setSourceRGB 0 0 0
       rectangle 0 0 w h
       fill
       -- Line configuration
       setLineWidth 1
       -- Render kd-tree
       renderKdTree kdt

renderKdTree :: KdTree -> Render ()
renderKdTree = KdTree.mapM_ r
             where r (Leaf q) = do setSourceRGB 0.5 0.5 0.5; renderQuad q
                   r (Node l c1 c2) = do
                     setSourceRGB 1 1 1
                     renderQuad $ Quad (o l) (d l)

coords :: Line -> (Double, Double, Double, Double)
coords (Line (Vec2 x1 y1) (Vec2 x2 y2)) =
       (fromIntegral x1, fromIntegral y1
       ,fromIntegral x2, fromIntegral y2)

renderQuad :: Quad -> Render ()
renderQuad (Quad (Vec2 x1 y1) (Vec2 x2 y2)) = do
           let w = x2-x1+1; h = y2-y1+1
           rectangle (db x1) (db y1) (db w) (db h)
           fill

db = fromIntegral