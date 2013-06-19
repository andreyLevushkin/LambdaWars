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
import Data.BoundingBox.B2

import Control.Applicative

import Core
import GeometryUtils
import Data.Label.Pure
import WorldRules
import TupleUtils

-- | Random instances used for generating bot starting positions
instance Random Point where
  random               = runRand $ Vector2 <$> getRandom <*> getRandom
  randomR (from, to)   = runRand $ Vector2 <$> (getRandomR ((v2x from), (v2x to)))
                                           <*> (getRandomR ((v2y from), (v2y to)))

instance Random (Double, Double) where
  random = runRand $ do v1 <- getRandom
                        v2 <- getRandom
                        return (v1,v2)
  
  randomR (from, to) = runRand $ do v1 <- getRandomR (fst from, fst to)
                                    v2 <- getRandomR (snd from, snd to)
                                    return (v1,v2)
  
instance Random BotState where
  random = runRand $ do 
    position <- getRandomR (Vector2 0 0, Vector2 arenaWidth arenaHeight)
    let zero = Vector2 0 0
      in return $ BotState position zero zero zero NoAction
                          
  randomR (from, to)   = runRand $ let zero  = Vector2 0 0
                                       fromP = get botPosition from :: Point
                                       toP   = get botPosition to
                                   in do 
                                     position <- getRandomR (fromP, toP)
                                     return $ BotState position zero zero zero NoAction
                                   
-- |Generates non overlapping bot states, we don't want to start with collisions
instance Random [BotState] where
  random g =  (nubBy nubCmp states, head gs)
    where 
      -- | Return true if bots are not colliding
      nubCmp s1 s2 = not $ botBotCollision s1 s2  
      (states, gs) = unzip $ unfoldr f g 
      f g          = Just ((next, gNext), gNext)
                       where 
                        (next, gNext) = random g
              
              
-- | Ensure that the bot can't turn by more then permitted by the rules etc.
sanitizeCommand :: Command -> Command
sanitizeCommand (Accelerate value) = Accelerate $ bound 0 maxAcceleration value
sanitizeCommand (Decelerate value) = Decelerate $ bound 0 maxDeceleration value
sanitizeCommand (MoveTurret value) = MoveTurret $ bound (-turretTurnRate) turretTurnRate value
sanitizeCommand (MoveRadar value)  = MoveRadar  $ bound (-radarTurnRate) radarTurnRate value
sanitizeCommand (Turn value)       = Turn       $ bound (-turnRate) turnRate value
sanitizeCommand cmd = cmd

bound :: Ord a => a -> a -> a -> a
bound x y = max x . min y
  
-- | Create a new BotState given a command issued by the bot
stepBotState :: Command -> BotState -> BotState
stepBotState cmd = apply cmd . moveBot
  where
    apply NoAction = id
    apply (Accelerate delta)   = modify botVelocity $ vmap (+delta)
    apply (Decelerate delta)   = modify botVelocity $ vmap (+ (-delta))
    apply (Turn       degrees) = modify botVelocity $ rotate degrees   
    apply (MoveTurret degrees) = modify botTurret   $ rotate degrees     
    apply (MoveRadar  degrees) = modify botRadar    $ rotate degrees 
    apply _ = id    
    moveBot state = modify botPosition (+ get botVelocity state) state

-- | Gather up all the Fire commands issued by bots and create bullets for them.
bulletsFired :: [(Step, BotState)] -> [Bullet]
bulletsFired bots = map (fire . snd) $ filter hasFired bots
  where hasFired (step, _) = stepCmd step == Fire

-- | Returns a bullet traveling the direction the bot turret is pointing
fire :: BotState -> Bullet
fire state = Bullet position velocity 
  where position = get botPosition state
        velocity = vnormalise (get botTurret state) |* (fromInteger bulletSpeed)

vLength :: Vector2 -> Double
vLength vector = sqrt $ vdot vector vector

-- | TODO Return true if the bots are colliding 
botBotCollision :: BotState -> BotState -> Bool
botBotCollision bot1 bot2 = (<10) . vLength $ pos bot1 - pos bot2
  where pos = get botPosition

-- | Returns true if this bot is colliding with a wall
botWallCollision :: BotState -> Bool
botWallCollision state =  (get botPosition state) `within_bounds` shrink 10 arenaBBox 

-- TODO scan results and collision results
newDashBoard :: [BotState] -> BotState -> DashBoard
newDashBoard otherBots bot = DashBoard NothingFound NoCollision (get botVelocity bot) 

-- | TODO this function steps the world - 
--   for now it does not hit test bullets or test for collisions
stepWorld :: World -> World
stepWorld (World bots bullets bbox) = World (zip newSteps newStates) newBullets bbox
  where         
    steps      = map mkSteps bots
    newBullets = map stepBullet $ bullets ++ bulletsFired steps
    commands   = map (sanitizeCommand . stepCmd . fst) steps
    newStates  = map (uncurry stepBotState ) $ zip commands $ map snd bots
    newSteps   = map (stepNext . fst) steps
    mkSteps bot@(automaton, state) = (step dashboard automaton, state)
      where dashboard = newDashBoard otherBots state
            otherBots = filter (== state) . map snd $ bots
            
-- | Step bullet
stepBullet :: Bullet -> Bullet                  
stepBullet bullet = modify bulletPosition (+ get bulletVelocity bullet) bullet

-- | Returns true if the match is over
matchIsOver :: World -> Bool  
matchIsOver (World bots _  _) = length bots < 2
  
-- | Generate a new random world with the supplied bots                                
newWorld :: RandomGen g => g -> [Bot a] -> World
newWorld gen bots = World (zip automata states) [] arenaBBox
  where states   = take (length bots) . fst $ random gen
        automata = map start bots                       
        zero     = Vector2 0 0

  
  
  
  
