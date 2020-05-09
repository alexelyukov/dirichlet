module Generator (
  generatePoints2d,
) where

import Types
import System.Random

type BeginPoint = Point2d
type EndPoint = Point2d
type Count = Int

generatePoints2d :: BeginPoint -> EndPoint -> (Point2d -> Bool) -> Count -> StdGen -> [Point2d]
generatePoints2d (Point2d x0 y0) (Point2d x1 y1) inShape count stdGen =
  let (stdGenX, stdGenY) = split stdGen
  in take count $ filter inShape $ zipWith Point2d (map (\x -> x0 + x * (x1 - x0)) $ randoms stdGenX) (map (\y -> y0 + y * (y1 - y0)) $ randoms stdGenY)