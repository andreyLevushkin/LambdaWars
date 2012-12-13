module Process where

import Control.Monad.Reader
import Control.Monad.Cont

-- | Bot process steps yield a single command and the entire rest of the computation
data Step m = Step { stepCmd :: Command, stepProc :: Process m }

-- | this is the infinite stream of steps from the Bot
newtype Process m = Process { runProcess :: m (Step m) }

-- | start the process. transforms a Bot into a Process stream
start :: Monad m => ContT (Step m) m a -> Process m
start bot = Process $ runContT bot $ const . fix $ \c -> return $ Step Empty $ Process c

yield :: Monad m => Command -> ContT (Step m) m ()
yield cmd = ContT $ \c -> return $ Step cmd (Process $ c ())

----------------------------------------------------------------------

data Command = Empty
             | Fire
             | Accelerate Double
             deriving Show

-- | this is the dashboard of readings, ie. the bots view
-- the bot is provided a new set of readings every step
data DashBoard = DashBoard { accl :: Double } -- TODO this just has a dummy value for now

type View = Reader DashBoard
type Bot = ContT (Step View) View ()

-- | runs the process for one step, requires the dashboard for this process
step :: DashBoard -> Process View -> Step View
step dash proc = runReader (runProcess proc) dash

bot1 :: Bot
bot1 = do
  yield Fire
  readings <- ask
  yield $ Accelerate $ accl readings
  yield Fire

-- A single step
test1 = stepCmd . step (DashBoard 0) $ start bot1

-- All three steps
-- TODO for now, we are feeding dummy dashboards into each process
test2 = take 3 $ map stepCmd $ iterate (step (DashBoard 0.5) . stepProc) s
  where
    s = step (DashBoard 0) (start bot1)

