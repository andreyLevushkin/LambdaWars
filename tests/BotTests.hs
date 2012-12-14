module BotTests where

import Control.Monad.Reader
import Control.Monad.Cont

import Core
import Arena

----------------------------------------------------------------------
-- Example bots


bot1 :: Bot
bot1 = do
  vel <- readVelocity
  cmdAccelerate 0.5
  cmdFire
  cmdDecelerate 0.5

dummyDash = DashBoard NothingFound 1.0 NoCollision

-- A single step
test1 = stepCmd . step dummyDash $ start bot1

-- All three steps
-- TODO for now, we are feeding dummy dashboards into each process
test2 = take 3 $ map stepCmd $ iterate (step dummyDash . stepProc) s
  where
    s = step dummyDash (start bot1)
