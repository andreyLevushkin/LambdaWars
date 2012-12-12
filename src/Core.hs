module Core where

import Control.Monad.Reader

type Point    = (Float, Float)
type Velocity = (Float, Float)

type Degree = Float

data BotState = BotState { 
  botPosition :: Point,
  botVelocity :: Point,
  botTurret   :: Degree,
  botRadar    :: Degree
}

data Bullet = Bullet {
  bulletPosition :: Point,
  bulletVelocity :: Point
  } deriving (Show)
              
data ScanResult = BotFound  Float
                | WallFound Float
                | NothingFound

data World  = World [(Bot,BotState)] [Bullet]
  
type Arena = Reader (BotState, ScanResult)

data Direction = Vertical | Horizontal


data Command   = Accelerate Direction Float
               | MoveTurret Degree
               | Fire
               | MoveRadar Degree
                 
                 
type Bot = () -- M (Bot, Command)

