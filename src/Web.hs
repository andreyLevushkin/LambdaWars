module Web (withServerDo, Broadcaster) where

import Snap.Http.Server
import Snap.Core
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, newChan, Chan, writeChan)
import Data.IORef
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 as B
import System.Directory (createDirectoryIfMissing)

import Core (World)

type Broadcaster = World -> IO ()

renderWorld :: IORef (Maybe World) -> Snap ()
renderWorld broadcastRef = do
	maybeWorld <- liftIO $ readIORef broadcastRef
	writeBS (B.pack (show maybeWorld))

site :: IORef (Maybe World) -> Snap ()
site broadcastRef =
	ifTop (writeBS "hello world") <|>
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
