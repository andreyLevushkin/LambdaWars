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