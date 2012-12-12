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
  


data Direction = Vertical | Horizontal


data Command   = Accelerate Direction Float
               | MoveTurret Degree
               | Fire
               | MoveRadar Degree
                 
                 


-- The monad inside which the bots will run.
newtype Arena a = Arena a
instance Monad Arena where
  -- TODO the monad for the arena


newtype Bot = Bot { runBot :: Arena (Command, Bot)  }
