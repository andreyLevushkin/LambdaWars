module GeometryUtils where

import Data.Vector.V2 
import Data.Vector.Transform.T2 
import Data.Angle 
import Data.Vector.Transform.Fancy 
import Data.Vector.Fancy 
import Data.Vector.Class
import Data.BoundingBox.B2

rotate :: Angle a => a Scalar -> Vector2 -> Vector2
rotate by = transformP2 (rotateT AxisX AxisY by)

shrink :: Double -> BBox2 -> BBox2
shrink delta box = bound_corners bottomLeft topRight
  where 
    bottomLeft = min_point box + (Vector2 delta delta)
    topRight   = max_point box - (Vector2 delta delta)

angleRads :: Fractional a => Vector2 -> Vector2 -> a
angleRads v1 v2 
    | v1 == (Vector2 0 0) = 0
    | v2 == (Vector2 0 0) = 0
    | otherwise           = realToFrac . acos $ vdot v1 v2 / ((vmag v1) * (vmag v2))

angleDegrees :: (Num a, Fractional a) => Vector2 -> Vector2 -> a
angleDegrees v1 v2 = radiansToDegrees $ angleRads v1 v2

radiansToDegrees :: (Fractional a ,Num a) => a -> a
radiansToDegrees = (*) (180 / (realToFrac pi))

xAxisVector = Vector2 1 0 

yAxisVector = Vector2 0 1