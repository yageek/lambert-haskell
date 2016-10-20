module Lambert (Unit, Point) where

-- The different unit
data Unit = Degree | Radian | Grad deriving(Eq)
-- Conversion values

degreeToradian = pi / 180.0
radianToDegree = 180.0 / pi

gradTodegree = 180.0 / 200.0
degreeTograd = 200.0 / 180.0

gradToradian = pi / 200.0
radiantTograd = 200.0 / pi

-- Define the point data constructor
data Point = Point Double Double Double Unit

unit :: Point -> Unit 
unit (Point _ _ _ unit) = unit

-- Scale the point
scale :: Point -> Double -> Point
scale (Point x y z unit) sc = Point (x*sc) (y*sc) (z*sc) unit 

-- Convert to degree
degree :: Point -> Point
degree pt 
    | u == Degree = pt
    | u == Grad = pt
    | u == Radian = pt
    where u = unit pt
