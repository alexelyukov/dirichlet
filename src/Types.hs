module Types (
  Radius,
  Point2d( .. ),
  Circle( .. )
) where

type Radius = Double

data Point2d = Point2d Double Double deriving (Show)
data Circle = Circle Point2d Radius deriving (Show)
