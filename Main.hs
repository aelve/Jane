{-# LANGUAGE
  RankNTypes, ConstraintKinds, DataKinds #-}


-- | We should be able to quit at any time whatsoever, which means that no
-- special handling of quitting is allowed.  After all, power outages don't
-- leave a chance to “confirm exit”.
module Main where


import           ClassyPrelude hiding (putStrLn, getLine)

import           ClassesLens
import           Command
import           GUI.GTK
import           IPC
import           Helpers
import           N
import           Notes
import           DB


main :: IO ()
main = do
  -- Needed so that 'mainThreadId' and 'niceExit' would work.
  registerMainThread

  running ← callRecv' "popup"
  unless running $ runGTK $ runDB $ runAux emptyAuxState stack


stack :: TAux (DBT (GTKT IO)) ()
stack =
  -- When threads want to kill the program, they call 'mainExitSuccess',
  -- which merely throws an exception to the main thread.
  flip onException (finishRecv >> finishGUI) $ do
    startRecv [("popup", \[] → popupWindow)]
    args ← liftIO' getArgs
    dbLocation .= DefaultDBFile
    loadDB
    case args of
      [] → do
        setTitle "Aelve Jane, #25.07.2014"
        repl
      s  → execute $ unwords s


repl :: M ()
repl = do
  command ← getLine ">"
  execute command
  repl
