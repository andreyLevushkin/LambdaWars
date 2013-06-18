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
    

-- Examples

demoBBox2 = bound_corners (Vector2 100 100) (Vector2 500 500)

demoShrink = shrink 10 demoBBox2

-- Assuming x = 0 and y = 0 is in the top-left, x axis is left-to-right
-- and y axis is top-to-bottom. Then point is 100 right of top left.
-- demoRotate is a clock-wise rotatation by 45 degress. 
demoRotate = rotate (Degrees 45) (Vector2 100 0)