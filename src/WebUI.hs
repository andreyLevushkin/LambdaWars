{-# LANGUAGE OverloadedStrings #-}

module WebUI where

import Network.HTTP.Server as HTTP
import Network.HTTP.Server.Logger

import Codec.Binary.UTF8.String as UTF8 (encodeString)
import Network.URI

import Data.IORef

import Text.Html

import Text.Blaze.Svg
import Text.Blaze.Renderer.String (renderMarkup)

import UI
import SimpleBots
import Engine
import Core
import System.Random

todo = undefined

webUI :: IO ()
webUI = do
    firstWorld <- createWorld
    worldRef <- todo
    serverWith webConfig (webHandler worldRef)

webConfig :: HTTP.Config
webConfig = defaultConfig { srvLog = stdLogger }


-- type Handler a = SockAddr -> Network.URL.URL -> Request a -> IO (Response a)
webHandler :: (IORef World) -> HTTP.Handler String
webHandler worldRef = \statusCode url request ->
            case rqMethod request of
                GET -> case (uriPath $ rqURI request) of
                            "/world.svg" -> do
                                                t <- todo
                                                world <- todo
                                                return (sendSVG OK (renderWorldToSvg world))
                            otherwise -> return (sendHTML OK todo)
                otherwise -> return (sendHTML NotFound $ thehtml noHtml)

-- Game Helper Functions
                
createWorld :: IO World
createWorld = do
    gen <- getStdGen
    let bots = [rammingBot, searchAndFire, searchAndFire, runInCircle]
    return (newWorld gen bots)
                
-- Web Server Helper Functions
                
sendHTML :: StatusCode -> Html -> Response String
sendHTML statusCode html = insertHeader HdrContentType "text/html"  -- Add the header and
             $ sendText statusCode (renderHtml html)                -- then send as text

sendSVG :: StatusCode -> Svg -> Response String
sendSVG statusCode svg = insertHeader HdrContentType "image/svg+xml" -- Add the header and
            $ sendText statusCode (renderMarkup svg)                 -- then send as text

sendText :: StatusCode -> String -> Response String
sendText statusCode txt = insertHeader HdrContentLength (show (length encodedTxt))
              $ insertHeader HdrContentEncoding "UTF-8"
              $ insertHeader HdrContentEncoding "text/plain"
              $ (respond statusCode :: Response String) { rspBody = encodedTxt }
  where encodedTxt = UTF8.encodeString txt
