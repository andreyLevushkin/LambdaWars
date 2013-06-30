{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Engine (runBattle) where

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
        unit = Vector2 1 0
      in return $ BotState "" position zero unit unit NoAction
                          
  randomR (from, to)   = runRand $ let zero  = Vector2 0 0
                                       unit  = Vector2 1 0
                                       fromP = get botPosition from :: Point
                                       toP   = get botPosition to
                                   in do 
                                     position <- getRandomR (fromP, toP)
                                     return $ BotState "" position zero unit unit NoAction
                                   
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
              
  randomR (from, to)   = undefined -- TODO - replace with better definition
              
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
updateBotState :: (Command, BotState) -> BotState
updateBotState (cmd, state) = apply cmd . moveBot $ state
  where
    apply (Accelerate delta)   = modify botVelocity $ vmap (+delta)
    apply (Decelerate delta)   = modify botVelocity $ vmap (+ (-delta))
    apply (Turn       degrees) = modify botVelocity $ rotate degrees   
    apply (MoveTurret degrees) = modify botTurret   $ rotate degrees     
    apply (MoveRadar  degrees) = modify botRadar    $ rotate degrees 
    apply _                    = id

    moveBot state = modify botPosition (+ get botVelocity state) state

-- | Gather up all the Fire commands issued by bots and create bullets for them.
bulletsFired :: [(Step, BotState)] -> [Bullet]
bulletsFired bots = map (fire . snd) $ filter hasFired bots
  where hasFired (step, _) = stepCmd step == Fire

-- | Returns a bullet traveling the direction the bot turret is pointing
fire :: BotState -> Bullet
fire state = Bullet position velocity 
  where
  -- When setting the position make sure the bullet starts off outside the bot
  position      = get botPosition state + ((botSize / 2 + 1) *|  unit_velocity) 
  velocity      = unit_velocity |* (fromInteger bulletSpeed)
  unit_velocity = vnormalise (get botTurret state)

-- | Returns the length of the vector
vLength :: Vector2 -> Double
vLength vector = sqrt $ vdot vector vector

-- | TODO Return true if the bots are colliding 
botBotCollision :: BotState -> BotState -> Bool
botBotCollision bot1 bot2 = (>10) . vLength $ pos bot1 - pos bot2
  where pos = get botPosition

-- | Returns false if this bot is colliding with a wall
isNotWallCollision :: BotState -> Bool
isNotWallCollision state = x > halfBot && y > halfBot 
                            && x < arenaWidth - halfBot && y < arenaHeight - halfBot
    where 
      (Vector2 x y) = (get botPosition state) 
      halfBot       = botSize / 2

-- TODO scan results and collision results
newDashBoard :: [BotState] -> BotState -> DashBoard
newDashBoard otherBots bot = DashBoard NothingFound NoCollision (get botVelocity bot) 

-- | TODO this function steps the world - 
--   for now it does not hit test bullets or test for collisions
stepWorld :: World -> World
stepWorld (World bots bullets bbox) = World survivors newBullets bbox
  where         
    steps       = map (stepBot bots) bots
    newBullets  = map stepBullet $ bullets ++ bulletsFired steps
    commands    = map (sanitizeCommand . stepCmd . fst) steps
    newStates   = map updateBotState $ zip commands $ map snd bots
    newAutomata = map (stepNext . fst) steps
    survivors   = pruneDeadBots newBullets $ zip newAutomata newStates

-- | Given a list of bots and bullets in flight prune out dead bots that are 
--   for example hit or collided with walls.
pruneDeadBots :: [Bullet]  -> [(Automaton, BotState)] -> [(Automaton, BotState)]
pruneDeadBots bullets bots = bulletSurvivors
  where 
    bulletSurvivors = filter (\bot->all (isNotBulletHit bot) bullets) wallSurviros
    wallSurviros    = filter (isNotWallCollision . snd) bots

-- | TODO - For now this assumes the bots are circular which does not match the 
--   the graphics so updating for square bots would be good.
isNotBulletHit :: (Automaton, BotState) -> Bullet -> Bool
isNotBulletHit (_, state) bullet = vLength bulletDistance > botSize
  where bulletDistance = (get botPosition state) - (get bulletPosition bullet)

-- | Given a list of all the bots in the world and a single bot. Steps a single 
--   bot to generate a Step that can be incorporated into the world.
--   Deals with generating the dashboard the bot uses for steps.
stepBot :: [(Automaton, BotState)] -> (Automaton, BotState) -> (Step, BotState)
stepBot bots bot@(automaton, state) = (step dashboard automaton, state)
  where dashboard = newDashBoard otherBots state
        otherBots = filter (== state) . map snd $ bots
            
-- | Step bullet
stepBullet :: Bullet -> Bullet                  
stepBullet bullet = modify bulletPosition (+ get bulletVelocity bullet) bullet

-- | Returns the match result for a given world 
--   
matchIsOver :: World -> MatchResult  
matchIsOver (World bots _  _) = case bots of
    (_, winner):[] -> Won $ get botName winner
    []             -> Draw 
    _              -> Ongoing . map  (get botName) . snd . unzip $ bots
  
-- | Generate a new random world with the supplied bots. The string in the tuple 
--   gives each bot a name.                              
newWorld :: RandomGen g => g -> [(String, Bot a)] -> World
newWorld gen namedBots = World (zip automata states) [] arenaBBox
  where 
    (names, bots) = unzip namedBots
    states        = take (length bots) . fst $ random gen
    namedStates   = map  (uncurry $ set botName) $ zip names states
    automata      = map start bots                       

-- | This is the only function exported by this module. Given a UI and a list of 
--   bots it plays out the battle using the UI and then prints out the final 
--   result.
runBattle :: UI -> [(String, Bot a)] -> IO ()
runBattle ui bots = do
  gen <- getStdGen
  let world = newWorld gen bots
  result <- runUI ui world stepWorld matchIsOver
  putStrLn $ "Match result is:" ++ show result


  
  
  
