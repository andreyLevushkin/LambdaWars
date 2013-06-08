-- For now I removed snap from the dependency list to reduce the number of build dependencies so this file will not compile
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, newChan, Chan, writeChan)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Char8 as B
import Data.IORef
import Data.Monoid (mempty)
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe (serveFile)
import System.Directory (createDirectoryIfMissing)
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Core (World)
import UI (renderWorldToSvg)

type Broadcaster = World -> IO ()

renderWorld :: IORef (Maybe World) -> Snap ()
renderWorld broadcastRef = liftIO (readIORef broadcastRef) >>= writeLazyText . renderSvg . maybe mempty renderWorldToSvg

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
