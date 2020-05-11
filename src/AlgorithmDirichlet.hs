module AlgorithmDirichlet (
  getTriangleMagicPoint,
  getNodes,
) where

import Types
import Utils
import Data.List as L
import Geometry

getTriangleMagicPoint :: Triangle -> Point2d
getTriangleMagicPoint (Triangle (Point2d ax ay) (Point2d bx by) (Point2d cx cy)) =
  let d = 2 * (ax * (by - cy) + bx * (cy - ay) + cx * (ay - by))
      centerX = ((ax ^ 2 + ay ^ 2) * (by - cy) + (bx ^ 2 + by ^ 2) * (cy - ay) + (cx ^ 2 + cy ^ 2) * (ay - by)) / d
      centerY = ((ax ^ 2 + ay ^ 2) * (cx - bx) + (bx ^ 2 + by ^ 2) * (ax - cx) + (cx ^ 2 + cy ^ 2) * (bx - ax)) / d
  in Point2d centerX centerY

getNodes :: [Point2d] -> [Triangle] -> [Node]
getNodes [] _ = []
getNodes (p:ps) ts =
  let triangles = filter (\(Triangle x1 x2 x3) -> x1 ~= p || x2 ~= p || x3 ~= p) ts
      trianglePoints = filter (~/= p) $ uniq $ concat $ [[t1, t2, t3] | (Triangle t1 t2 t3) <- triangles]
      magicPoints = polarAngleSort p $ map getTriangleMagicPoint triangles
  in Node p trianglePoints (clearMagicPoints magicPoints) : getNodes ps ts

polarAngleSort :: Point2d -> [Point2d] -> [Point2d]
polarAngleSort x0 = L.sortBy (sortPredicat x0)

sortPredicat :: Point2d -> Point2d -> Point2d -> Ordering
sortPredicat p0 p1 p2 =
  let angle1 = getAbsoluteAngle p0 p1
      angle2 = getAbsoluteAngle p0 p2
  in compare angle1 angle2
