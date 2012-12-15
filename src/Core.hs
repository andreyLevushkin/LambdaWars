{-# LANGUAGE ViewPatterns #-}
module Core where

import Control.Monad.Reader
import Control.Monad.Cont

-- | Bot process steps yield a single command and the entire rest of the computation
data Step m = Step { stepCmd :: Command, stepNextProc :: Process m }

-- | this is the infinite stream of steps from the Bot
newtype Process m = Process { runProcess :: m (Step m) }

-- | start the process. transforms a Bot into a Process stream
start :: Monad m => ContT (Step m) m a -> Process m
start bot = Process $ runContT bot $ const . fix $ \c -> return $ Step Empty $ Process c

yield :: Monad m => Command -> ContT (Step m) m ()
yield cmd = ContT $ \c -> return $ Step cmd (Process $ c ())

----------------------------------------------------------------------

type View = Reader DashBoard
type Arena a = ContT (Step View) View a -- keep Andrey's name for this
type Bot = ContT (Step View) View ()
type BotProcess = Process View
type BotStep = Step View

-- | runs the process for one step, requires the dashboard for this process
runProc :: DashBoard -> BotProcess -> BotStep
runProc dash proc = runReader (runProcess proc) dash


-- | this is the dashboard of readings, ie. the bots view
-- the bot is provided a new set of readings every step
data DashBoard = DashBoard {
    dashRadar     :: ScanResult
  , dashVelocity  :: Double
  , dashBearing   :: Degree
  , dashWallHit   :: Collision
  }
                 
data Collision = Collision | NoCollision

type Point    = (Double, Double)
type Velocity = (Double, Double)
type Degree = Double

-- | this is the full state that we keep for each bot
data BotState = BotState {
  botPosition :: Point,
  botVelocity :: Point,
  botBearing  :: Degree,
  botTurret   :: Degree,
  botRadar    :: Degree,
  botLastCmd  :: Command -- ^ useful for logging
} deriving Show

data Bullet = Bullet {
  bulletPosition :: Point,
  bulletVelocity :: Point
  } deriving (Show)
              
data ScanResult = BotFound Double
                | WallFound Double
                | NothingFound

type BoundingBox = (Point, Point)
data World  = World [(BotProcess, BotState)] [Bullet] BoundingBox

-- TODO: actual useful Show instance, just for testing webserver currently
instance Show World where
  show = const "World"

data Command  = Empty
              | Turn Degree
              | Accelerate Double
              | Decelerate Double
              | MoveTurret Degree
              | MoveRadar Degree
              | Fire
                 deriving Show

-- TODO nicer way of extracting states
states :: World -> [BotState]
states (World (unzip -> (_, states')) _ _) = states'

stepBotState :: Command -> BotState -> BotState
stepBotState cmd s = s' { botLastCmd = cmd }
  where
    s' = case cmd of
      Empty  -> s
      Turn d -> s { botBearing = botBearing s + d }
      _  -> s -- TODO add remaining command cases
      -- need arithmetic on points
      -- use e.g. velocity verlet
  
-- | use the bot state to create the dashboard readings
-- TODO radar and collision not yet implemented
newDash :: BotState -> DashBoard
newDash s = DashBoard NothingFound (sqmag $ botVelocity s) 0 NoCollision
  where
    sqmag (x, y) = sqrt $ x*x + y*y

-- | runs the world for one step
-- TODO we do not handle bullets or collisions for now
stepWorld :: World -> World
stepWorld (World (unzip -> (botProcs, botStates)) bullets box)
  = World (zip botProcs' botStates') bullets box 
  where
    dashes = map newDash botStates
    botSteps = zipWith runProc dashes botProcs
    cmds = map stepCmd botSteps
    botProcs' = map stepNextProc botSteps
    botStates' = zipWith stepBotState cmds botStates

-- | start the bots and create a new world
-- TODO bounding box
initWorld :: [Bot] -> World
initWorld bots = World (zip botProcs botStates) [] undefined
  where
    botProcs = map start bots
    botStates = iterate id $ BotState (0,0) (0,0) 0 0 0 Empty
