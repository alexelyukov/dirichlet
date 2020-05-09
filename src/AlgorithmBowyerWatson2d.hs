module AlgorithmBowyerWatson2d (
  getAroundCircle,
) where

import Types
import Data.List

type Distance = Double

getAroundCircle :: [Point2d] -> Circle
getAroundCircle = getCircleByDiagonal . getMaxDiagonal

getCircleByDiagonal :: (Point2d, Point2d, Distance) -> Circle
getCircleByDiagonal (Point2d x1 y1, Point2d x2 y2, distantion) =
  let xm = (x1 + x2) / 2
      ym = (y1 + y2) / 2
  in Circle (Point2d xm ym) (distantion / 2)

getMaxDiagonal :: [Point2d] -> (Point2d, Point2d, Distance)
getMaxDiagonal [x] = (x, x, 0)
getMaxDiagonal (x:xs) =
  let calcDistance (Point2d x1 y1) (Point2d x2 y2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2
      maxPredicat (x1, y1, d1) (x2, y2, d2) = compare d1 d2
  in maximumBy maxPredicat $ getMaxDiagonal xs : [(x, y, calcDistance x y) | y <- xs]