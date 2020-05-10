module Drawer2d (
  drawBackground,
  drawCircle,
  drawTriangles,
  drawTriangle,
  drawRectangle,
  drawLine,
  drawPoints,
  drawPoint
) where

import Types
import GHC.Float
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Codec.Picture(PixelRGBA8( .. ), Image)

drawBackground :: Drawing PixelRGBA8 () -> Image PixelRGBA8
drawBackground = 
    let backgroundColor = PixelRGBA8 255 255 255 255
        drawColor = PixelRGBA8 0x00 0x00 0x00 255
        width = 2000
        height = 2000
    in renderDrawing width height backgroundColor . withTexture (uniformTexture drawColor)

drawCircle :: Circle -> Drawing PixelRGBA8 ()
drawCircle c =
  let (pc', r') = prepareCircle c
  in stroke 1 JoinRound (CapRound, CapRound) $ circle pc' r'

drawTriangles :: [Triangle] -> Drawing PixelRGBA8 ()
drawTriangles = mconcat . map drawTriangle

drawTriangle :: Triangle -> Drawing PixelRGBA8 ()
drawTriangle (Triangle p1 p2 p3) = do
  drawLine p1 p2
  drawLine p2 p3
  drawLine p3 p1

drawRectangle :: Rectangle -> Drawing PixelRGBA8 ()
drawRectangle (Rectangle (Point2d x0 y0) (Point2d x1 y1)) = do
  drawLine (Point2d x0 y0) (Point2d x1 y0)
  drawLine (Point2d x1 y0) (Point2d x1 y1)
  drawLine (Point2d x1 y1) (Point2d x0 y1)
  drawLine (Point2d x0 y1) (Point2d x0 y0)

drawLine :: Point2d -> Point2d -> Drawing PixelRGBA8 ()
drawLine p0 p1 = stroke 1 JoinRound (CapRound, CapRound) $ line (preparePoint2d p0) (preparePoint2d p1)

drawPoints :: [Point2d] -> Drawing PixelRGBA8 ()
drawPoints = mconcat . map drawPoint

drawPoint :: Point2d -> Drawing PixelRGBA8 ()
drawPoint pc =
    let pointColor = PixelRGBA8 0x00 0x00 0x00 255
    in withTexture (uniformTexture pointColor) $ fill $ circle (preparePoint2d pc) 2

prepareCircle :: Circle -> (Point, Float)
prepareCircle (Circle pc r) = (preparePoint2d pc, double2Float r)

preparePoint2d :: Point2d -> Point
preparePoint2d (Point2d x y) = V2 (double2Float x) (double2Float y)