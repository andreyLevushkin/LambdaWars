module UI (showBoard) where

import Text.Blaze.Svg (Svg)
import Core (BotState, Bullet)

showBoard :: [BotState] -> [Bullet] -> IO ()
showBoard = undefined

renderSvg :: [BotState] -> [Bullet] -> Svg 
renderSvg = undefined

renderBot :: BotState -> Svg
renderBot = undefined

renderBullet :: Bullet -> Svg
renderBullet = undefined

server :: IO ()
server = undefined 

javascript :: String
javascript = undefined
