module Engine where

import System.Random
import Control.Monad.Random
import Control.Monad.Random.Class
import Control.Monad.Loops
import Data.List
import Data.Label.Pure

import Debug.Trace

-- Vectors
--import Data.Vector.Fancy 
import Data.Vector.V2 
import Data.Vector.Transform.T2 
import Data.Angle 
import Data.Vector.Transform.Fancy 
import Data.Vector.Class

import Control.Applicative

import Core
import GeometryUtils
import Data.Label.Pure
import WorldRules

-- TODO randomR
instance Random Point where
  random g = runRand build g
    where build = Vector2 <$> getRandom <*> getRandom

instance Random (Double, Double) where
  random g = runRand build g
    where build = do
            v1 <- getRandom
            v2 <- getRandom
            return (v1,v2)
            
instance Random BotState where
  random g = runRand build g
    where build = do
            position <- getRandom
            let zero = Vector2 0 0
            return $ BotState position zero zero zero NoAction

-- | Ensure that the bot can't turn by more then permitted by the rules.
sanitizeCommand :: Command -> Command
sanitizeCommand = id

-- | Create a new BotState given a command issued by the bot
stepBotState :: Command -> BotState -> BotState
stepBotState NoAction state = state
stepBotState (Accelerate delta)   state = modify botVelocity (vmap (+delta) )    state 
stepBotState (Decelerate delta)   state = modify botVelocity (vmap (+ (-delta))) state 
stepBotState (Turn       degrees) state = modify botVelocity (rotate degrees)    state 
stepBotState (MoveTurret degrees) state = modify botTurret   (rotate degrees)    state 
stepBotState (MoveRadar  degrees) state = modify botRadar    (rotate degrees)    state 

bulletsFired :: [(Step, BotState)] -> [Bullet]
bulletsFired bots = map (fire . snd) $ filter hasFired bots
  where hasFired (step, _) = stepCmd step == Fire

fire :: BotState -> Bullet
fire state = Bullet position velocity 
  where position = get botPosition state
        velocity = vnormalise (get botTurret state) |* (fromInteger bulletSpeed)

-- TODO
botBotCollision :: BotState -> BotState -> Bool
botBotCollision bot1 bot2 = False

-- TODO
botWallCollision :: BotState -> Bool
botWallCollision state = False

-- Generates non overlapping bots states
instance Random [BotState] where
  random g =  (nubBy botBotCollision states, head gs)
    where 
      (states, gs) = unzip $ unfoldr f g 
      f g = Just ((next, gNext), gNext)
        where (next, gNext) = random g
        
                          

-- TODO scan results and collision results
newDashBoard :: [BotState] -> BotState -> DashBoard
newDashBoard otherBots bot = DashBoard NothingFound NoCollision (get botVelocity bot) 

-- TODO Hit test bots - for now just kill off one bot per tick
pruneDeadBots :: World -> World
pruneDeadBots =  modify worldBots tail 

stepWorld :: World -> World
stepWorld (World bots bullets bbox) = World (zip (map (stepNext . fst) steps) newStates) newBullets bbox
  where f (process, state) = (step (newDashBoard (map snd bots) state) process, trace (show state) state)
        steps = map f bots
        newBullets = map stepBullet $ bullets ++ bulletsFired steps
        commands = map (stepCmd . fst) steps
        newStates  = map (uncurry stepBotState ) $ zip commands $ map snd bots

stepBullet :: Bullet -> Bullet                  
stepBullet bullet = modify bulletPosition (+ get bulletVelocity bullet) bullet

matchIsOver :: World -> Bool  
matchIsOver (World bots _  _) = length bots < 2
  
runMatch :: [Bot a] -> IO (Maybe (Bot a))
runMatch bots = do 
  startStates <- randomIO >>= return . take (length bots) :: IO [BotState]
  let world   = World (zip botProcesses startStates) [] arenaBBox
      botProcesses = map start bots
      result = until matchIsOver ( pruneDeadBots . stepWorld) world
  print result
  return Nothing
  
  
  
  
