module IPC (
  startRecv, callRecv, callRecv', finishRecv )
  where


import           ClassyPrelude
import qualified Prelude as P
import           Control.Concurrent.Lifted (fork, forkOS, killThread)
import           Control.Monad.Trans.Control
import           Data.BERT
import           Network.BERT
import           Text.Printf
import           Control.Concurrent.Chan

import           Helpers


-- | Starts a server on TCP port @60180@. The server executes requests
-- specified as a list of pairs @(functionName, function)@. An example of a
-- function:
-- 
-- @
-- add [IntTerm a, IntTerm b] = liftIO' $ putStrLn (tshow (a + b))
-- @
-- 
-- TODO: Functions can't return anything yet.
-- 
-- TODO: The server might be accessible from outside network.
-- 
-- TODO: solve the problem of multiple users running the app simultaneously
-- (which is valid behavior).
startRecv :: (MonadBaseControl IO m, IO' m) =>
             [(String, [Term] → m ())] → m ()
startRecv table = do

  -- 'serve' operates in pure IO and not in any-monad-supporting-IO. Until
  -- this is fixed, we create a channel for accepting function names and
  -- arguments from the server, and then process them in a loop which runs in
  -- a separate thread.

  -- Create the channel.
  ch ← liftIO' newChan

  let
    -- When a request for our module comes...
    dispatch "Jane"  funcName args = do
      -- ...we send it to the channel and report success (even if the
      -- function doesn't exist).
      writeChan ch (funcName, args)
      return $ Success $ BoolTerm True

    -- In a weird case when the request came for some another module...
    dispatch modName funcName _    = do
      -- ...we refuse to accept it and print an error message.
      printf "IPC: no such module %s." modName
      return NoSuchModule

  -- Create the socket.
  serv ← liftIO' $ tcpServer 60180
  
  -- Start the server in a separate thread.
  servThreadID ← liftIO' $ forkOS $ serve serv dispatch      

  -- This is the loop which actually does the job. It works in a generic
  -- IO-monad.
  let procLoop = do
        -- Receive the function name and arguments.
        (funcName, args) ← liftIO' $ readChan ch
        -- Print a debug message.
        liftIO' $ printf "IPC: received %s." funcName
        if | funcName == "finishRecv" →
               killThread servThreadID
           | Just f ← lookup funcName table → do
               f args
               procLoop
           | otherwise → do
               liftIO' $ printf "IPC: no such function %s." funcName
               procLoop

  -- Run the loop in a separate thread.
  fork procLoop
  return ()


-- | Calls a function on the server without any arguments. Returns 'True' if
-- the server is running and has accepted the call.
callRecv' :: String → IO_ Bool
callRecv' funcName = callRecv funcName ([] :: [Term])


-- | Calls a function on the server. Returns 'True' if the server is running
-- and has accepted the call.
callRecv :: BERT a => String → [a] → IO_ Bool
callRecv funcName args = liftIO' $ do
  eTrans ← tryAny $ tcpClient "localhost" 60180
  case eTrans of
    -- Any error at all happened? Nice, we probably aren't running.
    -- TODO: what kind of errors can happen here?
    Left _ → return False
    -- *Someone* is running there, let's talk to them.
    Right trans → do
      res ← call trans "Jane" funcName args
      closeConnection trans
      case res of
        Right True → return True
        -- Something happened.
        -- TODO: what kind of errors can happen here?
        _          → return False


-- | Sends a message to the server which kills it.
finishRecv :: IO_ ()
finishRecv = void $ callRecv' "finishRecv"
