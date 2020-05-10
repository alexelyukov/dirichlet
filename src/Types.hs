module Types (
  Radius,
  Point2d( .. ),
  Circle( .. ),
  Triangle( .. ),
  Rectangle( .. ),
  Edge( .. ),
  (~=),
  (~/=),
) where

class ApproximatelyEqual a where
  (~=) :: a -> a -> Bool
  (~/=) :: a -> a -> Bool
  x ~/= y = not (x ~= y)

type Radius = Double

data Point2d = Point2d Double Double deriving (Eq, Show)
data Circle = Circle Point2d Radius deriving (Show)
data Triangle = Triangle Point2d Point2d Point2d deriving (Eq, Show)
data Rectangle = Rectangle Point2d Point2d deriving (Show)
data Edge = Edge Point2d Point2d deriving (Eq, Show)

instance ApproximatelyEqual Point2d where
  Point2d p1 p2 ~= Point2d m1 m2 = sqrt ((p1 - m1) ^ 2 + (p2 - m2) ^ 2) < 1e-4

instance ApproximatelyEqual Edge where
  Edge p1 p2 ~= Edge m1 m2 = (p1 ~= m1 && p2 ~= m2) || (p1 ~= m2 && p2 ~= m1)

instance ApproximatelyEqual Triangle where
  Triangle p1 p2 p3 ~= Triangle m1 m2 m3 =
    (p1 ~= m1 && p2 ~= m2 && p3 ~= m3)
    || (p1 ~= m1 && p2 ~= m3 && p3 ~= m2)
    || (p1 ~= m2 && p2 ~= m1 && p3 ~= m3)
    || (p1 ~= m2 && p2 ~= m3 && p3 ~= m1)
    || (p1 ~= m3 && p2 ~= m1 && p3 ~= m2)
    || (p1 ~= m3 && p2 ~= m2 && p3 ~= m1)