module GeometryUtils where

import Data.Vector.V2 
import Data.Vector.Transform.T2 
import Data.Angle 
import Data.Vector.Transform.Fancy 
import Data.Vector.Fancy 
import Data.Vector.Class

rotate :: Angle a => a Scalar -> Vector2 -> Vector2
rotate by = transformP2 (rotateT AxisX AxisY by)