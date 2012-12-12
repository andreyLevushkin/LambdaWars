module Command where

import Control.Monad.Reader
import Control.Monad.Cont

-- | this is the infinite stream of commands from the Bot
-- we could use a "Free Monad" structure instead, if we want to add Stop
newtype Command m = Command { runCommand :: m (Command m) }

-- | start the process. transforms a Bot into a Command stream
start :: Monad m => ContT (Command m) m a -> Command m
start x = Command $ runContT x $ const . fix $ \c -> return (Command c)

-- | transforms a computation in the monad m into an atomic command in Cont m a
command :: Monad m => m a -> ContT (Command m) m a
command m = ContT $ \c -> return $ Command $ m >>= c

----------------------------------------------------------------------
-- Example use of the above

-- as an example, we'll wrap IO
type Bot = ContT (Command IO) IO ()

-- runs the started processes for one step
stepAll :: [Command IO] -> IO [Command IO]
stepAll = mapM runCommand

bot1 :: Bot
bot1 = do
  command (putStrLn "1")
  lift $ putStrLn "2"
  command (putStrLn "3")

bot2 :: Bot
bot2 = do
  command (putStrLn "Foo")
  command (putStrLn "Bar")

-- | runs two steps: start and first commands
-- uncomment out the comment to get another command
main = do
  s1 <- stepAll [start bot1, start bot2]
  s2 <- stepAll s1
  --stepAll s2
  return ()
