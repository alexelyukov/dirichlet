module Graham (
    getMinPoint,
    filterPoints,
    polarAngleSort,
    fullatan,
    genGraham
) where

import Graphics.Rasterific
import Data.List



getMinPoint :: [Point] -> Maybe Point
getMinPoint [] = Nothing
getMinPoint [x] = Just x
getMinPoint (x:xs) = minPointByY x (getMinPoint xs)

minPointByY :: Point -> Maybe Point -> Maybe Point
minPointByY (V2 x0 y0) (Just (V2 x1 y1))
  | y0 > y1  = Just (V2 x1 y1)
  | y0 < y1  = Just (V2 x0 y0)
  | x0 < x1 = Just (V2 x0 y0)
  | x0 > x1 = Just (V2 x1 y1)



filterPoints :: Maybe Point -> [Point] -> [Point]
filterPoints Nothing xs = xs
filterPoints (Just x) xs = filter (/= x) xs



polarAngleSort :: Maybe Point -> [Point] -> [Point]
polarAngleSort Nothing _ = []
polarAngleSort (Just minPoint) xs = sortBy (sortPredicat minPoint) xs

sortPredicat :: Point -> Point -> Point -> Ordering
sortPredicat (V2 minx miny) (V2 x0 y0) (V2 x1 y1)
  | miny == y0 && y0 == y1 = compare x0 x1
  | miny == y0 && y0 /= y1 = LT
  | miny == y1 && y0 /= y1 = GT

  | minx == x0 && x0 == x1 = compare y1 y0
  | minx == x0 && x0 /= x1 = compare x1 minx
  | minx == x1 && x0 /= x1 = compare minx x0
  | otherwise = compare (fullatan $ (y0 - miny) / (x0 - minx)) (fullatan $ (y1 - miny) / (x1 - minx))

fullatan :: Float -> Float
fullatan x
  | x < 0 = pi + atan x
  | otherwise = atan x



genGraham :: Maybe Point -> [Point] -> [Point]
genGraham Nothing _ = []
genGraham (Just minPoint) xs = []