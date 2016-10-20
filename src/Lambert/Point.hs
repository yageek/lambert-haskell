module Lambert.Point (Unit, Point, degree, radian, grad)  where
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
    | u == Grad = scale pt gradTodegree
    | u == Radian = scale pt radianToDegree
    where u = unit pt

-- Convert to radian
radian :: Point -> Point
radian pt 
    | u == Degree = scale pt degreeToradian
    | u == Grad = scale pt gradToradian
    | u == Radian = pt
    where u = unit pt

-- Convert to grad
grad :: Point -> Point
grad pt 
    | u == Degree = scale pt degreeTograd
    | u == Grad = pt
    | u == Radian = scale pt radiantTograd
    where u = unit pt

