module Main where

-- import Codec.Picture(writePng)
-- import Graphics.Rasterific
import System.Random
import Types
import Generator
import AlgorithmBowyerWatson2d
-- import Lib
-- import Graham

main :: IO ()
main = do
  gen <- newStdGen

  print $
    let points = generatePoints2d (Point2d 500 500) (Point2d 1500 1500) inShape 10 gen
    in getAroundCircle points

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
  let radius = 500
      (centerX, centerY) = (1000, 1000)
  in (x - centerX) ^ 2 + (y - centerY) ^ 2 <= radius ^ 2