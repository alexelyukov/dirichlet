module AlgorithmBowyerWatson2d (
  getAroundTriangle,
  calcTriangulation,
  removeTriangle,
  isPointInCircle,
  getEdgesFromTriangle,
  isNotSharedEdge
) where

import Types
import Data.List

type Distance = Double

getAroundTriangle :: [Point2d] -> Triangle
getAroundTriangle = getAroundTriangleByCircle . getCircleByRectangle . getRectangleByPoints

getAroundTriangleByCircle :: Circle -> Triangle
getAroundTriangleByCircle (Circle (Point2d xc yc) radius) =
  let p1 = Point2d (xc - sqrt 3 * radius) (yc + radius)
      p2 = Point2d (xc + sqrt 3 * radius) (yc + radius)
      p3 = Point2d xc (yc - 2 * radius)
  in Triangle p1 p2 p3

getCircleByRectangle :: Rectangle -> Circle
getCircleByRectangle (Rectangle (Point2d x0 y0) (Point2d x1 y1)) =
  let xm = (x0 + x1) / 2
      ym = (y0 + y1) / 2
      radius = 0.5 * sqrt ((x1 - x0) ^ 2 + (y1 - y0) ^ 2)
  in Circle (Point2d xm ym) radius
getRectangleByPoints :: [Point2d] -> Rectangle
getRectangleByPoints [x] = Rectangle x x
getRectangleByPoints (x:xs) =
  let blowRectangle (Point2d x' y') (Rectangle (Point2d x0 y0) (Point2d x1 y1)) = Rectangle (Point2d (min x0 x') (min y0 y')) (Point2d (max x1 x') (max y1 y'))
  in blowRectangle x $ getRectangleByPoints xs

makeTriangleByEdgeAndPoint :: Edge -> Point2d -> Triangle
makeTriangleByEdgeAndPoint (Edge (Point2d x1 y1) (Point2d x2 y2)) (Point2d x3 y3) = Triangle (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3)

getEdgesFromTriangle :: Triangle -> [Edge]
getEdgesFromTriangle (Triangle (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3)) = [
  Edge (Point2d x1 y1) (Point2d x2 y2),
  Edge (Point2d x2 y2) (Point2d x3 y3),
  Edge (Point2d x3 y3) (Point2d x1 y1)]

isNotSharedEdge :: Edge -> [Triangle] -> Bool
isNotSharedEdge e ts = and $ mconcat [map (e ~/=) (getEdgesFromTriangle t) | t <- ts]

isPointInCircle :: Point2d -> Triangle -> Bool
isPointInCircle (Point2d x y) (Triangle (Point2d ax ay) (Point2d bx by) (Point2d cx cy)) =
  let d = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
      centerX = ((ax ^ 2 + ay ^ 2) * (by - cy) + (bx ^ 2 + by ^ 2) * (cy - ay) + (cx ^ 2 + cy ^ 2) * (ay - by)) / d
      centerY = ((ax ^ 2 + ay ^ 2) * (cx - bx) + (bx ^ 2 + by ^ 2) * (ax - cx) + (cx ^ 2 + cy ^ 2) * (bx - ax)) / d
      ab = sqrt ((ax - bx) ^ 2 + (ay - by) ^ 2)
      bc = sqrt ((bx - cx) ^ 2 + (by - cy) ^ 2)
      ca = sqrt ((cx - ax) ^ 2 + (cy - ay) ^ 2)
      p = (ab + bc + ca) / 2
      radius = (ab * bc * ca) / (4 * sqrt (p * (p - ab) * (p - bc) * (p - ca)))
  in (x - centerX) ^ 2 + (y - centerY) ^ 2 <= radius ^ 2

calcTriangulation :: Triangle -> [Point2d] -> [[Triangle]]
calcTriangulation triangle ps =
  let triangulation = scanl addPointToTriangulation [triangle] ps
  -- in removeTriangle triangle triangulation
  in triangulation

addPointToTriangulation :: [Triangle] -> Point2d -> [Triangle]
addPointToTriangulation triangulation p =
  let badTriangles = filter (isPointInCircle p) triangulation
      polygons = mconcat [filter (`isNotSharedEdge` filter (t ~/=) badTriangles) (getEdgesFromTriangle t) | t <- badTriangles]
      cleanTriangulation = mconcat $ map (`removeTriangle` triangulation) badTriangles
      newTriangulation = map (`makeTriangleByEdgeAndPoint` p) polygons ++ cleanTriangulation
  in newTriangulation

removeTriangle :: Triangle -> [Triangle] -> [Triangle]
removeTriangle (Triangle s1 s2 s3) = filter (\(Triangle x1 x2 x3) -> and $ (~/=) <$> [s1, s2, s3] <*> [x1, x2, x3])