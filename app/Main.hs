module Main where

import Codec.Picture(writePng)
import System.Random
import Types
import Generator
import AlgorithmBowyerWatson2d
import Drawer2d
import Data.List
import Geometry
import AlgorithmDirichlet

main :: IO ()
main = do
  gen <- newStdGen
  let generatedpoints = generatePoints2d (Point2d 250 250) (Point2d 1750 1750) inShape 400 gen
      surfacePoints = getSurfacePoints 20
      points = generatedpoints ++ surfacePoints
      triangle = getAroundTriangle points
      triangulation = calcTriangulation triangle points
      magicPoints = map getTriangleMagicPoint triangulation
      nodes = getNodes points triangulation

  print $ checkTriangulation triangulation points

  writePng "image.png" $ drawBackground $ do
    drawCircle $ Circle (Point2d 1000 1000) 750
    drawNodes nodes
