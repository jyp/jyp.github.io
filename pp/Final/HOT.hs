

data Point = Point Float Float
type Region = Point -> Bool

(.+.) :: Point -> Point -> Point
Point x1 y1 .+. Point x2 y2 = Point (x1 + x2) (y1 + y2)

opposite :: Point -> Point
opposite (Point x y) = Point (negate x) (negate y)

(.-.) :: Point -> Point -> Point
p1 .-. p2 = p1 .+. opposite p2

norm2 :: Point -> Float
norm2 (Point x y) = x*x + y*y

outside :: (Point -> Bool) -> Point -> Bool
outside r p = not (r p)

(∩) :: Region -> Region -> (Point -> Bool)
r1 ∩ r2 = \p -> r1 p && r2 p

withinRange :: Float -> Point -> (Point -> Bool)
withinRange range p1 p2 = norm2 (p1 .-. p2) <= range * range

engagementZone :: Point -> Region
engagementZone p = withinRange 30 p ∩ outside (withinRange 10 p)

