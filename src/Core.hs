module Core  where

import Control.Monad.Reader
import Control.Monad.Cont

-- | Bot process steps yield a single command and the entire rest of the computation
data Step m = Step { stepCmd :: Command, stepProc :: Process m }

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
step :: DashBoard -> BotProcess -> BotStep
step dash proc = runReader (runProcess proc) dash


-- | this is the dashboard of readings, ie. the bots view
-- the bot is provided a new set of readings every step
data DashBoard = DashBoard {
    dashRadar    :: ScanResult
  , dashVelocity :: Double
  , dashWallHit  :: Collision
  }
                 
data Collision = Collision | NoCollision

type Point    = (Double, Double)
type Velocity = (Double, Double)

type Degree = Double

-- | this is the full state that we keep for each bot
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
              
data ScanResult = BotFound  Double
                | WallFound Double
                | NothingFound

data World  = World [(BotProcess,BotState)] [Bullet]

-- TODO: actual useful Show instance, just for testing webserver currently
instance Show World where
  show = const "World"

data Command  = Empty
              | Accelerate Double
              | Decelerate Double
              | Turn Degree
              | MoveTurret Degree
              | Fire
              | MoveRadar Degree
                 deriving Show