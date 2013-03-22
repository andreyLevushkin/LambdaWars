module Main where

import Core
import Web
import Engine

import SimpleBots

import System.Random
import Control.Concurrent (threadDelay)
import Data.Vector.V2
import Data.BoundingBox.B2

serverLoop :: World -> Broadcaster -> IO ()
serverLoop world broadcast = do
	-- XXX: just stubbed out atm, wait 1 second then broadcast an empty world
	threadDelay 1000000
        broadcast world
        if matchIsOver world
          then serverLoop world broadcast
          else serverLoop (stepWorld world) broadcast
        
main = do
	withServerDo $ \broadcast -> do
		putStrLn "Web server up and running"
                gen <- getStdGen
                let bots = [rammingBot, searchAndFire, searchAndFire, runInCircle]
                  in serverLoop (newWorld gen bots) broadcast
