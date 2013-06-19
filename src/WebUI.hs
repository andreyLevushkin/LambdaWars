
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

-- TODO = Add code to allow quiting the game in GHCi without closing GHCi

gameUI :: IO ()
gameUI = do
    firstWorld <-createWorld
    worldRef <- newIORef firstWorld
    serverWith webConfig (lambdaWarsHandler worldRef)

webConfig :: HTTP.Config
webConfig = defaultConfig { srvLog = stdLogger }


-- lambdaWarsHandler takes a IORef to the world and returns the HHTP Handler.
-- If it is GET for the SVG return the SVG.
-- If it is another GET, then return the home page
-- Otherwise reutrn 404 error

-- type Handler a = SockAddr -> URL -> Request a -> IO (Response a)
-- Everytime there is a HTTP request the HHTP Handler is called with 3 values:
-- 1) The detials of the socket used for the request -- TODO = check if it includes the client side details
-- 2) The URL of the request
-- 3) The details of the request
-- then the handler can do some IO (any IO it likes) and 
lambdaWarsHandler :: (IORef World) -> HTTP.Handler String
lambdaWarsHandler worldRef = \statusCode url request ->
            case rqMethod request of
                GET -> case (uriPath $ rqURI request) of
                            "/world.svg" -> do
                                                modifyIORef worldRef stepWorld -- Run 1 step of game
                                                nextWorld <- readIORef worldRef -- read the new world
                                                return (sendSVG OK (renderWorldToSvg nextWorld))
                                                                        -- TODO  Add time to SVG rendering
                            otherwise -> return (sendHTML OK homePage)
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

-- Html for home page, including the JavaScript

homePage =
        thehtml <<
            (header << Html [jQueryLink, renderScript]
            +++ body << noHtml)
            
scriptLink :: String -> HtmlElement
scriptLink link = scriptTag [("src", link)] ""

inlineScript :: String -> HtmlElement
inlineScript code = scriptTag [] code

scriptTag :: [(String, String)] -> String -> HtmlElement
scriptTag attrs content =
        HtmlTag "SCRIPT" (map mkAttr attrs) (Html [HtmlString content])
    where mkAttr (n, v) = HtmlAttr n v

jQueryLink = scriptLink "https://ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.min.js"

renderScript = inlineScript
    ("$.ajaxSetup ({ cache: false });" ++
        "$(function() { function render() { " ++
        "$.get(\"/world.svg\", function(svg) { " ++ 
        "$(\"body\").html(svg.documentElement); }); } " ++
        "setInterval(render, 50); });")
