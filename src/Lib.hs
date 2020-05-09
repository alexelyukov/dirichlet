module Lib (
    renderBackground,
    renderStrokeCircle,
    renderLine,
    renderPoint,
    generatePoints,
    genDelaunayTriangulation
) where

import Codec.Picture(PixelRGBA8( .. ), Image)
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import System.Random

type Radius = Float

data Node = Node Point [Point]

renderBackground :: Drawing PixelRGBA8 () -> Image PixelRGBA8
renderBackground = 
    let backgroundColor = PixelRGBA8 255 255 255 255
        drawColor = PixelRGBA8 0x00 0x00 0x00 255
        width = 1000
        height = 1000
    in renderDrawing width height backgroundColor . withTexture (uniformTexture drawColor)

renderStrokeCircle :: Point -> Radius -> Drawing PixelRGBA8 ()
renderStrokeCircle (V2 xc yc) r = stroke 1 JoinRound (CapRound, CapRound) $ circle (V2 xc yc) r

renderLine :: Point -> Point -> Drawing PixelRGBA8 ()
renderLine (V2 x0 y0) (V2 x1 y1) = stroke 1 JoinRound (CapRound, CapRound) $ line (V2 x0 y0) (V2 x1 y1)

renderPoint :: Point -> Drawing PixelRGBA8 ()
renderPoint (V2 xc yc) =
    let pointColor = PixelRGBA8 0x00 0x00 0x00 255
    in withTexture (uniformTexture pointColor) $ fill $ circle (V2 xc yc) 2

generatePoints :: Point -> Point -> Int -> StdGen -> [Point]
generatePoints (V2 x0 y0) (V2 x1 y1) count stdGen =
    let (stdGenX, stdGenY) = split stdGen
    in take count $ filter inShape $ zipWith V2 (map (\x -> x0 + x * (x1 - x0)) $ randoms stdGenX) (map (\y -> y0 + y * (y1 - y0)) $ randoms stdGenY)

inShape :: Point -> Bool
inShape (V2 x y) = 
    let radius = 500
        (centerX, centerY) = (500, 500)
    in (x - centerX) ^ 2 + (y - centerY) ^ 2 <= radius ^ 2

genDelaunayTriangulation :: [Point] -> Float
genDelaunayTriangulation [V2 x0 y0, V2 x1 y1, V2 x2 y2] = 10
genDelaunayTriangulation [V2 x0 y0, V2 x1 y1, V2 x2 y2, V2 x3 y3] = 10