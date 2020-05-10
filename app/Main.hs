module Main where

import Codec.Picture(writePng)
import System.Random
import Types
import Generator
import AlgorithmBowyerWatson2d
import Drawer2d
import Data.List

main :: IO ()
main = do
  gen <- newStdGen
  let points = generatePoints2d (Point2d 500 500) (Point2d 1500 1500) inShape 20000 gen
      triangle = getAroundTriangle points
      triangulation = calcTriangulation triangle points


  writePng "image.png" $ drawBackground $ do
    drawCircle $ Circle (Point2d 1000 1000) 500
    drawPoint (Point2d 1000 1000)
    drawPoints points
    drawTriangles triangulation



inShape :: Point2d -> Bool
inShape (Point2d x y) =
  let radius = 500
      (centerX, centerY) = (1000, 1000)
  in (x - centerX) ^ 2 + (y - centerY) ^ 2 <= radius ^ 2