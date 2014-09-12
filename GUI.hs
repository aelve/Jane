{-# LANGUAGE
  UndecidableInstances, OverlappingInstances #-}


module GUI (
  MonadGUI(..),
  Output(..) )
  where


import           ClassyPrelude hiding (getLine)
import           Control.Monad.Trans
import           Control.Monad.Base


class Monad m => MonadGUI m where
  getLine          :: Text → m Text
  getLine'         :: Text → m (Maybe Text)
  setPrompt        :: Text → m ()
  output           :: Output → m ()
  setTitle         :: Text → m ()
  setInput         :: (Text, Text) → m ()
  finishGUI        :: m ()


instance (MonadTrans t, Monad (t m), MonadGUI m) =>
         MonadGUI (t m) where
  getLine x   = lift $ getLine x
  getLine' x  = lift $ getLine' x
  setPrompt x = lift $ setPrompt x
  output x    = lift $ output x
  setTitle x  = lift $ setTitle x
  setInput x  = lift $ setInput x
  finishGUI   = lift $ finishGUI

instance (MonadBase b m, MonadGUI b) =>
         MonadGUI m where
  getLine x   = liftBase $ getLine x
  getLine' x  = liftBase $ getLine' x
  setPrompt x = liftBase $ setPrompt x
  output x    = liftBase $ output x
  setTitle x  = liftBase $ setTitle x
  setInput x  = liftBase $ setInput x
  finishGUI   = liftBase $ finishGUI


data Output =
  OutText Text |
  OutMany [Output] |
  Info Output |
  NumberedList [Output]
