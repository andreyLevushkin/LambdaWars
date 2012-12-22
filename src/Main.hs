module Main where

import Core
import Web

import Control.Concurrent (threadDelay)
import Data.Vector.V2
import Data.BoundingBox.B2

serverLoop :: Broadcaster -> IO ()
serverLoop broadcast = do
	-- XXX: just stubbed out atm, wait 1 second then broadcast an empty world
	threadDelay 1000000
        broadcast $ World [] [ Bullet (Vector2 100 50) undefined ] (BBox2 0 0 100 100)
	serverLoop broadcast

main = do
	withServerDo $ \broadcast -> do
		putStrLn "Web server up and running"
		serverLoop broadcast
