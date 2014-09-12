{-# LANGUAGE
  TemplateHaskell, ConstraintKinds, DataKinds, TypeFamilies #-}


-- | This module provides “fake” GUI which allows testing commands without
-- the GTK overhead.
module GUI.Fake (
  Fake', Fake, FakeT, runFake, GUI.MonadGUI(..),
  enableMessages, addInputLines )
  where


import           ClassyPrelude hiding (getLine, putStrLn)
import qualified ClassyPrelude as P
import           Control.Monad.Classes
import           Control.Monad.State (StateT, evalStateT)
import           Control.Monad.Loops (iterateUntil)
import           Text.Printf
import           Control.Concurrent.Chan
import           Control.Concurrent.STM.TChan
import           Control.Concurrent (threadDelay)

import qualified GUI as GUI
import           Helpers
import           ClassesLens


-- | A queue of lines. It has to be a 'TChan' because we need 'tryReadTChan'
-- for 'getLine'.
type LQueue = TChan Text


-- | Internal state of GUI-handling code.
data FakeState = FakeState {
  _inputQueue :: LQueue,      -- ^ Queue holding all lines entered so far.
  _messages   :: Bool }       -- ^ Flag determining whether 'putStr',
                              -- 'setPrompt' and so on should output
                              -- anything. False by default.

makeLenses ''FakeState


-- We need Fake' and Fake'' separated to avoid code duplication (see the
-- signature of runFake).
type Fake'' m = IO' m


-- | Monads under this constraint have to provide access to 'IO' and be
-- instances of @'MonadState' 'FakeState'@.
type Fake' m = (Fake'' m, MonadState FakeState m)


-- | A type for actions with access to the GUI. See 'Fake''.
type Fake a = Fake' m => m a


-- | A 'Fake' monad transformer.
type FakeT = StateT FakeState


instance Fake'' m => GUI.MonadGUI (FakeT m) where
  getLine          = getLine
  getLine'         = getLine'
  setPrompt        = setPrompt
  putStr           = putStr
  putStrLn         = putStrLn
  setTitle         = setTitle
  setInput         = setInput
  getTerminalWidth = getTerminalWidth
  finishGUI        = finishFake


-- | Prepares the command input entrybox.
-- 
-- The logic of the entrybox is as follows:
-- 
--   * When Enter is pressed in the TextView, we have to interpret it as
--   “submit command” no matter what's the cursor position, because otherwise
--   the line will get broken and interpreted as 2 commands.
-- 
--   * When multiple lines are pasted into empty box, they are treated as
--   multiple commands.
-- 
--   * Otherwise, they're concatenated (with spaces as separators).



runFake :: Fake'' m => FakeT m a → m a
runFake gui = do
  lQueue ← liftIO' newTChanIO
  let state = FakeState {
        _inputQueue = lQueue,
        _messages   = False }
  evalStateT gui state


-- | Turns on output messages by 'setPrompt', 'putStr' and so on.
enableMessages :: Fake ()
enableMessages = messages .= True


-- | Prints an output message (see 'enableMessages').
message :: String → Fake ()
message t = do
  enabled ← use messages
  when enabled $ liftIO' $ P.putStrLn $ pack t


setPrompt :: Text → Fake ()
setPrompt s = message $ printf "GUI.Fake: prompt = %s" (show s)


finishFake :: Fake ()
finishFake = message $ printf "GUI.Fake: finished"


putStr :: Text → Fake ()
putStr t = message $ printf "GUI.Fake: printed %s" (show t)


putStrLn :: Text → Fake ()
putStrLn t = putStr (t ++ "\n")


setTitle :: Text → Fake ()
setTitle s = message $ printf "GUI.Fake: title = %s" (show s)


setInput :: (Text, Text) → Fake ()
setInput (left, right) =
  message $ printf "GUI.Fake: input = %s/%s" (show left) (show right)


getLine' :: Text → Fake (Maybe Text)
getLine' p = do
  setPrompt p
  queue ← use inputQueue
  liftIO' $ atomically $ tryReadTChan queue


getLine :: Text → Fake Text
getLine p = do
  setPrompt p
  queue ← use inputQueue
  liftIO' $ atomically $ readTChan queue


getTerminalWidth :: Fake Int
getTerminalWidth = return 78


-- | Adds lines to the input queue.
addInputLines :: [Text] → Fake ()
addInputLines ts = do
  queue ← use inputQueue
  mapM_ (liftIO' . atomically . writeTChan queue) ts
