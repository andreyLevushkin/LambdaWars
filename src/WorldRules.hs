
-- | This file defines the rules of the LambdaWars universe

module WorldRules where

import Data.BoundingBox.B2
import Data.Angle

-- |Turn rate in degrees per tick
turnRate :: Degrees Double
turnRate = 5

-- |Radar turn rate in degrees per tick
radarTurnRate :: Degrees Double
radarTurnRate  = 15

-- |Turret turn rate in degrees per tick
turretTurnRate :: Degrees Double
turretTurnRate = 10

-- |Max speed in pixles per tick
maxSpeed       = 8

-- |Max acceleration in pixels per tick per tick
maxAcceleration :: Double
maxAcceleration = 1

-- |Max deceleration in pixels per tick per tick
maxDeceleration :: Double
maxDeceleration = 2

-- |Withd of the arena
arenaWidth     = 1000

-- |Height of the arena
arenaHeight    = 1000

-- |How fast the bullets travel
bulletSpeed = 40 

-- |Bot dimensions - make bots circular for now
botSize :: Double
botSize =  10

-- | Bounding box of the arena
arenaBBox = BBox2 0 0 arenaHeight arenaWidth

