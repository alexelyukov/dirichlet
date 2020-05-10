module Main where

import Codec.Picture(writePng)
-- import Graphics.Rasterific
import System.Random
import Types
import Generator
import AlgorithmBowyerWatson2d
import Drawer2d
import Data.List
-- import Lib
-- import Graham

main :: IO ()
main = do
  -- gen <- newStdGen
  let points = generatePoints2d (Point2d 600 800) (Point2d 1400 1600) inShape 20 (mkStdGen 10)
      -- triangle = getAroundTriangle points
      triangle = Triangle (Point2d 100 1900) (Point2d 1900 1900) (Point2d 1000 100)
      triangulation = calcTriangulation triangle points

      triangulation11 = last (take 2 triangulation)
      tr1 = head triangulation11
      tr3 = last triangulation11

      edges = getEdgesFromTriangle tr1
      ed3 = last edges

      polygons = [filter (`isNotSharedEdge` [tr3]) [ed3] | t <- [tr1]]

      f = isNotSharedEdge ed3 [tr3]
      f1 = [map (ed3 ~/=) (getEdgesFromTriangle t) | t <- [tr1]]


  -- print points
  -- print triangle
  -- print triangulation11
  -- print $ polygons
  print $ tr3
  print $ getEdgesFromTriangle tr3
  print $ ed3
  print $ f
  -- print $ f1
  -- print $ edg1111
  -- print $ edg1 /= edg2

  writePng "image.png" $ drawBackground $ do
    -- drawCircle $ Circle (Point2d 1000 1200) 400
    -- drawPoint (Point2d 1000 1200)
    drawPoints points
    -- drawTriangle triangle
    drawTriangles $ triangulation11

  -- writePng "image.png" $ renderBackground $ do
  --   renderStrokeCircle (V2 1000 1000) 500
  --   renderLine (V2 1000 1000) (V2 1100 1100)
  --   renderPoint (V2 1000 1000)
  --   mconcat $ map renderPoint $ generatePoints (V2 500 500) (V2 1500 1500) 20 gen

  -- print $
  --   let points = generatePoints (V2 500 500) (V2 1500 1500) 100 gen
  --       minPoint = getMinPoint points
  --       Just (V2 minx miny) = minPoint
  --       filtredPoints = filterPoints minPoint points
  --       sortedPoints = polarAngleSort minPoint filtredPoints
  --   in map (\ (V2 x y) -> fullatan ((y - miny) / (x - minx))) sortedPoints
  -- print $ genDelaunayTriangulation [V2 0 0, V2 0 0, V2 0 0]


inShape :: Point2d -> Bool
inShape (Point2d x y) =
  let radius = 400
      (centerX, centerY) = (1000, 1200)
  in (x - centerX) ^ 2 + (y - centerY) ^ 2 <= radius ^ 2