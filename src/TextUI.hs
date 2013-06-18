module TextUI where

import SimpleBots
import Engine
import Core
import System.Random
import Text.PrettyPrint

textUI :: IO()
textUI = do
    firstWorld <- createWorld
    gameLoop firstWorld

gameLoop :: World -> IO ()
gameLoop world = do
    display world
    input <- getLine -- pause. TODO = Get a number turns to run or Q to exit
    let nextWorld = stepWorld world
    gameLoop nextWorld
    
createWorld :: IO World
createWorld = do
    gen <- getStdGen
    let bots = [rammingBot, searchAndFire, searchAndFire, runInCircle]
    return (newWorld gen bots)

display :: Pretty a => a -> IO()
display = putStrLn . render . ppr
