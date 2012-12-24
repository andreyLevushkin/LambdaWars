{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
module Core  where

import Control.Monad.Reader
import Control.Monad.Cont
import Data.Vector.V2
import Data.Angle
import Data.Label
import Data.BoundingBox.B2


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
  , dashWallHit   :: Collision
  , dashVelocity  :: Velocity
  }
                 
data Collision = Collision | NoCollision

type Point     = Vector2
type Velocity  = Vector2
type Direction = Vector2 

-- | this is the full state that we keep for each bot
data BotState = BotState {
  _botPosition :: Point,
  _botVelocity :: Point,
  _botTurret   :: Direction,
  _botRadar    :: Direction,
  _botLastCmd  :: Command -- ^ useful for logging
} deriving (Show, Eq)


type Degree = Degrees Double

data Bullet = Bullet {
  _bulletPosition  :: Point,
  _bulletVelocity  :: Velocity
  } deriving (Show)
              

data ScanResult = BotFound Double
                | WallFound Double
                | NothingFound

type BoundingBox = BBox2
data World  = World {
   _worldBots    :: [(Automaton, BotState)], 
   _worldBullets :: [Bullet],
   _worldBox     :: BoundingBox
}

-- TODO: actual useful Show instance, just for testing webserver currently
instance Show World where
  show (World bots bullets _) = show $ map snd bots

data Command  = NoAction
              | Turn Degree
              | Accelerate Double
              | Decelerate Double
              | MoveTurret Degree
              | MoveRadar Degree
              | Fire
              deriving (Show, Eq)

-- TODO nicer way of extracting states
states :: World -> [BotState]
states (World (unzip -> (_, states')) _ _) = states'

mkLabels [ ''BotState, ''Bullet, ''World ]                       


