
{-# LANGUAGE FlexibleInstances, ViewPatterns, TemplateHaskell #-}
module Core  where

import Control.Monad.Reader
import Control.Monad.Cont
import Data.Vector.V2
import Data.Angle
import Data.Label
import Data.BoundingBox.B2

-- Used for pretty printing
import Text.PrettyPrint
import Text.Printf  

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

data MatchResult = Ongoing [String]
                 | Draw
                 | Won String
                 deriving (Show)

-- | This is a data type used to abstract the UI allowing us to have pluggable 
--   UIs. For example we can have a web based UI using Snap or HTTP-server or 
--   an OpenGL based UI. 
-- 
--   To create a new UI you will need to create an instance of this type. This 
--   type contains a single function called runUI. It will get passwed the 
--   initial state of the worl the function to step the world and a function to
--   to check the status of the match. Using this info your implementation should 
--   display the match and when it's over return the result.
-- 
--   The Engine module requires a UI to display the bot match.
newtype UI = UI { 
    runUI :: World -> (World -> World) -> (World -> MatchResult) -> IO MatchResult 
  } 

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
  _botName     :: String,
  _botPosition :: Point,
  _botVelocity :: Point,
  _botTurret   :: Direction,
  _botRadar    :: Direction,
  _botLastCmd  :: Command -- ^ useful for logging
} deriving (Eq)

instance Show BotState where
  show = render . ppr

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

instance Show World where
  show = render . ppr

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

-------------------------------------------------------------------
-- Pretty printing for Core types ---------------------------------
-------------------------------------------------------------------

columnWidth :: Int
columnWidth = 20

lineWidth :: Int
lineWidth = columnWidth * 6

column :: String -> Doc
column = text . pad ' ' columnWidth

heading :: String -> Doc 
heading = text . pad '=' lineWidth . (++) "==" 

pad :: Char -> Int -> String -> String
pad c count input = input ++ (take padLength $ repeat c)
  where padLength = max 0 (count - length input)

class Pretty a where
  ppr :: a -> Doc

instance Pretty String where
  ppr = text 

instance Pretty Command where
  ppr = text . show

instance Pretty Vector2 where
  ppr (Vector2 x y) = column $ "(x:" ++ (printf "%.2f" x) ++ ", y:" ++ (printf "%.2f" y) ++ ")"

instance Pretty BotState where
  ppr (BotState name position velocity turret radar lastCmd) 
    = (column name) <+> (ppr position) <+> (ppr velocity) <+> (ppr turret) <+> (ppr radar) <+> (ppr lastCmd)

instance Pretty [BotState] where
  ppr bots = vcat $ (column "Name"      <+>
                     column "Position"  <+> 
                     column "Velocity"  <+> 
                     column "Turret"    <+> 
                     column "Radar"     <+> 
                     column "Last Command") : (map ppr bots)

instance Pretty [Bullet] where
  ppr bullets =  vcat $ (column "Position" <+> column "Velocity") : (map ppr bullets)

instance Pretty Bullet where
  ppr (Bullet position velocity) = (ppr position) <+> (ppr velocity)

instance Pretty World where
  ppr (World bots bullets arena) = vcat $ [
      heading "Bot arena status:",
      heading "Bots",
      ppr $ map snd bots,
      heading "Bullets",
      ppr bullets
    ]

mkLabels [ ''BotState, ''Bullet, ''World ]                       


