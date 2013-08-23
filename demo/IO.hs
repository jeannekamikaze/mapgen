module IO
(
    renderDecor
)
where

import Floor

import MapGen.Space

import Graphics.Rendering.Cairo

renderDecor :: Decoration -> Render ()
renderDecor (Room doors) = do
            setSourceRGB 0.2 0.2 1.0
            mapM_ renderDoor doors
renderDecor _ = return ()

renderDoor :: Door -> Render ()
renderDoor door = do
           let (x',y') = coords door
           rectangle x' y' 1 1
           fill

coords :: Vec2 -> (Double, Double)
coords (Vec2 x y) = (fromIntegral x, fromIntegral y)