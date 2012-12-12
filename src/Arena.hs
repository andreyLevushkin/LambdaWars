module Arena where

import Control.Monad.Reader.Class
import Control.Monad.Reader

import Core

-- Looking around your enviroment - takes no time
readRadar :: Arena ScanResult
readRadar = asks snd

readVelocity :: Arena Velocity
readvelocity = asks (botVelocity . fst)

readRadarPosition :: Arena Degree
readRadarPosition = asks (botRadar . fst)

readTturretPosition :: Arena Degree
readTturretPosition = asks (botTurret .fst)

readWallHit ::Arena Bool
readWallHit = undefined

-- Commands that take one tick
cmdTurnRight :: Arena ()
cmdTurnRight = undefined

cmdTurnLeft :: Arena ()
cmdTurnLeft = undefined

cmdAccelerate :: Arena ()
cmdAccelerate = undefined

cmdDecelerate :: Arena ()
cmdDecelerate = undefined

cmdTurnRadarLeft :: Arena ()
cmdTurnRadarLeft = undefined

cmdTurnRadarRight :: Arena ()
cmdTurnRadarRight = undefined

cmdTurnTurretLeft :: Arena ()
cmdTurnTurretLeft = undefined

cmdTurnTurretRight :: Arena ()
cmdTurnTurretRight = undefined

cmdFire :: Arena ()
cmdFire = undefined
