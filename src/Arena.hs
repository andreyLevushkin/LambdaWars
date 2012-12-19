{-# LANGUAGE ViewPatterns #-}
module Arena where

import Control.Monad.Reader.Class
import Control.Monad.Reader

import Core

-- Looking around your enviroment - takes no time

readRadar :: Bot ScanResult
readRadar = asks dashRadar

readVelocity :: Bot Double
readVelocity = asks dashVelocity

readBearing :: Bot Degree
readBearing = asks dashBearing

readWallHit ::Bot Collision
readWallHit = asks dashWallHit

-- Commands that take one tick
               
cmdTurn :: Degree -> Bot ()
cmdTurn = yield . Turn

cmdAccelerate :: Double -> Bot ()
cmdAccelerate = yield . Accelerate

cmdDecelerate :: Double -> Bot ()
cmdDecelerate = yield . Decelerate

cmdTurnRadar :: Degree -> Bot ()
cmdTurnRadar = yield . MoveRadar

cmdTurnTurret :: Degree -> Bot ()
cmdTurnTurret = yield . MoveTurret

cmdFire :: Bot ()
cmdFire = yield Fire

cmdNothing :: Bot ()
cmdNothing = yield NoAction
