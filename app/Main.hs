module Main where

import Codec.Picture(writePng)
import Graphics.Rasterific
import System.Random
import Lib
import Graham

main :: IO ()
main = do
  gen <- newStdGen

  writePng "image.png" $ renderBackground $ do
    renderStrokeCircle (V2 500 500) 500
    renderLine (V2 500 500) (V2 600 600)
    renderPoint (V2 500 500)
    mconcat $ map renderPoint $ generatePoints (V2 0 0) (V2 1000 1000) 1000 gen

  print $
    let points = generatePoints (V2 0 0) (V2 1000 1000) 100 gen
        minPoint = getMinPoint points
        Just (V2 minx miny) = minPoint
        filtredPoints = filterPoints minPoint points
        sortedPoints = polarAngleSort minPoint filtredPoints
    in map (\ (V2 x y) -> fullatan ((y - miny) / (x - minx))) sortedPoints
  -- print $ genDelaunayTriangulation [V2 0 0, V2 0 0, V2 0 0]