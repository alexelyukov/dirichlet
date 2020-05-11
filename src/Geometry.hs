module Geometry (
  inShape,
  getSurfacePoints,
  clearMagicPoints,
) where

import GHC.Float
import Types

inShape :: Point2d -> Bool
inShape (Point2d x y) =
  let radius = 750
      (centerX, centerY) = (1000, 1000)
  in (x - centerX) ^ 2 + (y - centerY) ^ 2 <= radius ^ 2

getSurfacePoints :: Int -> [Point2d]
getSurfacePoints n = do
  i <- [1..n]
  let radius = 750
      (centerX, centerY) = (1000, 1000)
      i' = int2Double i
      n' = int2Double n
      angle = 2 * pi * i' / n'
      x = centerX + radius * cos angle
      y = centerY + radius * sin angle
  return (Point2d x y)

clearMagicPoints :: [Point2d] -> [Point2d]
clearMagicPoints [] = []
clearMagicPoints [x0] = []
clearMagicPoints (x0:x1:xs)
  | inShape x0 && inShape x1 = x0:x1:clearMagicPoints xs
  | inShape x0 && not (inShape x1) = x0:calcIntersectPoint (Line2d x0 x1):clearMagicPoints xs
  | not (inShape x0) && inShape x1 = calcIntersectPoint (Line2d x0 x1):x1:clearMagicPoints xs
  | not (inShape x0) && not (inShape x1) = clearMagicPoints xs

calcIntersectPoint :: Line2d -> Point2d
calcIntersectPoint (Line2d (Point2d x0 y0) (Point2d x1 y1))
  | Point2d x0 y0 ~= Point2d x1 y1 = Point2d x0 y0
  | Point2d x0 y0 ~/= Point2d x1 y1 = calcIntersectPoint newLine
  where pm = Point2d ((x0 + x1) / 2) ((y0 + y1) / 2)
        newLine
          | inShape pm && inShape (Point2d x0 y0) = Line2d pm (Point2d x1 y1)
          | inShape pm && inShape (Point2d x1 y1) = Line2d (Point2d x0 y0) pm
          | not (inShape pm) && not (inShape (Point2d x0 y0)) = Line2d pm (Point2d x1 y1)
          | not (inShape pm) && not (inShape (Point2d x1 y1)) = Line2d (Point2d x0 y0) pm