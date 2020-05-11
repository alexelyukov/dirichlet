module Utils (
  uniq,
  getAbsoluteAngle,
) where

import Types

uniq :: [Point2d] -> [Point2d]
uniq [] = []
uniq (x:xs) = x : uniq (filter (~/= x) xs)

getAbsoluteAngle :: Point2d -> Point2d -> Double
getAbsoluteAngle (Point2d x1 y1) (Point2d x2 y2)
  | x > 0 = atan (y / x)
  | x < 0 = atan (y / x) + pi
  | x == 0 && y > 0 = pi / 2
  | x == 0 && y < 0 = 3 * pi / 2
  where x = x2 - x1
        y = y2 - y1