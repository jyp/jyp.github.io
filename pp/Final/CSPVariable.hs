import Control.Concurrent
import Control.Concurrent.Chan

data Command a = Get (Chan a) | Set a

type Variable a = Chan (Command a)

handler :: Variable a -> a -> IO ()
handler v a = do
  command <- readChan v
  case command of
    Set a' -> handler v a'
    Get c -> do 
      writeChan c a
      handler v a

newVariable :: a -> IO (Variable a)
newVariable a = do
  c <- newChan
  forkIO (handler c a)
  return c

get :: Variable a -> IO a
get v = do
  c <- newChan
  writeChan v (Get c)
  readChan c
  
set :: Variable a -> a -> IO ()
set v a = do
  writeChan v (Set a)
  

main = do
  v <- newVariable "initial value"
  set v "new value"
  get v >>= putStrLn
  
-- interactively  
