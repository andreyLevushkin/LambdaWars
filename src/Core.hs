{-# LANGUAGE ViewPatterns #-}
module Core where

import Control.Monad.Reader
import Control.Monad.Cont

-- | A Bot is a program for a bot. It is written in an imperative style
--   and is transformed into an Automoton for stepped execution.
type Bot = ContT Step (Reader DashBoard)

-- | Given a DashBoard, an Automaton emits a command and a new Automaton.
type Automaton = Reader DashBoard Step
data Step = Step { stepCmd :: Command, stepNext :: Automaton}

-- | Start a Bot, transforming it into an Automaton
start :: Bot a -> Automaton
start bot = runContT bot $ const . fix $ return . Step NoAction

-- | Yield a command for this step
yield :: Command -> Bot ()
yield cmd = ContT $ \c -> return $ Step cmd (c ())

-- | Run an Automaton to the next command, using the supplied dashboard
step :: DashBoard -> Automaton -> Step
step dash a = runReader a dash

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
data World  = World [(Automaton, BotState)] [Bullet] BoundingBox

-- TODO: actual useful Show instance, just for testing webserver currently
instance Show World where
  show = const "World"

data Command  = NoAction
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
      NoAction  -> s
      Turn d -> s { botBearing = botBearing s + d }
      _  -> s -- TODO add remaining command cases
      -- need arithmetic on points
      -- use e.g. velocity verlet
  
-- | use the bot state to create the dashboard readings
-- TODO radar and collision not yet implemented
newDashBoard :: BotState -> DashBoard
newDashBoard s = DashBoard NothingFound (sqmag $ botVelocity s) 0 NoCollision
  where
    sqmag (x, y) = sqrt $ x*x + y*y

-- | runs the world for one step
-- TODO we do not handle bullets or collisions for now
stepWorld :: World -> World
stepWorld (World (unzip -> (botAutos, botStates)) bullets box)
  = World (zip botAutos' botStates') bullets box 
  where
    dashes = map newDashBoard botStates
    botSteps = zipWith step dashes botAutos
    cmds = map stepCmd botSteps
    botAutos' = map stepNext botSteps
    botStates' = zipWith stepBotState cmds botStates

-- | start the bots and create a new world
-- TODO bounding box
initWorld :: [Bot ()] -> World
initWorld bots = World (zip botAutos botStates) [] undefined
  where
    botAutos = map start bots
    botStates = iterate id $ BotState (0,0) (0,0) 0 0 0 NoAction
