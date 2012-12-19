module BotTests where

import Control.Monad.Reader
import Control.Monad.Cont
import Control.Arrow

import Core
import Arena

----------------------------------------------------------------------
-- Example bots

bot1 :: Bot ()
bot1 = do
  cmdTurn 90
  deg <- readBearing
  cmdTurn $ deg + 90
  cmdAccelerate 0.5

bot2 :: Bot ()
bot2 = do
  cmdFire
  bot2

test = take 5 $ 
       map (map (botBearing &&& botLastCmd) . states) $ -- ^ strip out bearing
                                                        -- and command log
       iterate stepWorld $
       initWorld [bot1, bot2]

