module TextUI (textUI) where

import SimpleBots
import Engine
import Core
import System.Random
import Text.PrettyPrint

import System.Exit

textUI :: UI
textUI = UI $ \world stepper matchResult -> do

    -- Create a list of all the states in the match   
    let states =  iterate stepper world

    -- Print out the list of states to screen
    mapM_ (display matchResult) states

display :: (World -> MatchResult) -> World -> IO ()
display matchResult  world  = do
    print world 
    putStrLn  "Press ENTER to continue."
    getLine
    if (isFinished . matchResult) world
        then finish (matchResult world)
        else return ()

finish :: MatchResult -> IO ()
finish result = print result >> exitSuccess