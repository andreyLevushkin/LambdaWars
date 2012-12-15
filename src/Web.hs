module Web (withServerDo, Broadcaster) where

import Snap.Http.Server
import Snap.Core
import Snap.Util.FileServe (serveFile)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, newChan, Chan, writeChan)
import Data.IORef
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 as B
import System.Directory (createDirectoryIfMissing)

import Core (World)

instance Show World where
    show = const $ "<svg xmlns='http://www.w3.org/2000/svg' version='1.1'>"
              ++ "<circle cx='100' cy='50' r='40' stroke='black'"
              ++ "stroke-width='2' fill='red'/>"
              ++ "</svg>"

type Broadcaster = World -> IO ()

renderWorld :: IORef (Maybe World) -> Snap ()
renderWorld broadcastRef = do
    maybeWorld <- liftIO $ readIORef broadcastRef
    case maybeWorld of
        Just world -> writeBS (B.pack (show world))
        Nothing    -> return ()
	

site :: IORef (Maybe World) -> Snap ()
site broadcastRef =
	ifTop (serveFile "static/renderSVG.html") <|>
	route [
		("world.svg", renderWorld broadcastRef)
	]

withServerDo :: (Broadcaster -> IO ()) -> IO ()
withServerDo actionWithBroadcaster = do
	-- snap logs to stderr and says "THIS IS BAD" 
	-- if it can't create log files in ./log
	let createParents = False in createDirectoryIfMissing createParents "log"
	broadcastRef <- newIORef Nothing
	forkIO $ quickHttpServe (site broadcastRef)
	actionWithBroadcaster (\world -> writeIORef broadcastRef $ Just world)
