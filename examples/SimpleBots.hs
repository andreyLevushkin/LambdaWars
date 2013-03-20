module SimpleBots where

import Core
import Control.Monad
import Arena
import Engine

rammingBot :: Bot a
rammingBot = forever (cmdAccelerate 1)

searchAndFire :: Bot a
searchAndFire = do
  radar <- readRadar
  case radar of 
    BotFound _ -> cmdFire
    _          -> cmdTurnRadar 1
  searchAndFire
  
runInCircle :: Bot a
runInCircle = do
  cmdAccelerate 1
  cmdTurn 5
  runInCircle
  
  