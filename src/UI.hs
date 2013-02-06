{-# LANGUAGE OverloadedStrings #-}
module UI where

import Data.Label
import Data.Vector.V2

import Core (BotState, Bullet, botPosition, bulletPosition, World(..))
import Data.String (fromString)

import Text.Blaze.Svg (Svg)
import Text.Blaze.Svg11
import Text.Blaze.Svg11.Attributes

showBoard :: [BotState] -> [Bullet] -> IO ()
showBoard = undefined

renderSvg :: [BotState] -> [Bullet] -> Svg 
renderSvg = undefined

renderBot :: BotState -> Svg
renderBot bot =
    let vec = get botPosition bot
    in
        circle
           ! class_ "bot"
           ! cx (toValue.v2x $ vec)
           ! cy (toValue.v2y $ vec)
           ! r "5"

renderBullet :: Bullet -> Svg
renderBullet bullet =
    let vec = get bulletPosition bullet
    in
        circle
           ! class_ "bullet"
           ! cx (toValue.v2x $ vec)
           ! cy (toValue.v2y $ vec)
           ! r "5"

server :: IO ()
server = undefined 

javascript :: String
javascript = undefined

renderWorldToSvg :: World -> Svg
renderWorldToSvg (World bots bullets _) =
  docTypeSvg $ do
    mapM_ renderBullet bullets
    mapM_ renderBot (map snd bots)
