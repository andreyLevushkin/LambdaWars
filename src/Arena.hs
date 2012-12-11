module Arena where

import Control.Monad.Reader.Class
import Control.Monad.Reader

import Core


scan :: Arena ScanResult
scan = asks snd

position :: Arena Point
position = asks (botPosition . fst)

velocity :: Arena Velocity
velocity = asks (botVelocity . fst)

radarPosition :: Arena Degree
radarPosition = asks (botRadar . fst)

turretPosition :: Arena Degree
turretPosition = asks (botTurret .fst)