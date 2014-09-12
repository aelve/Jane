{-# LANGUAGE FlexibleContexts #-}

module TTest where

--import LLE
--import Control.Monad.Layer
--import Control.Monad.Interface.State as LS
import Control.Monad.State
import Control.Monad.Identity
import Control.Lens
import MTLEvil

inc :: MonadState Int m => m Int
inc = do
  id += 1
  use id

toggle :: MonadState Bool m => m Bool
toggle = do
  id %= not
  use id

blah :: IO Bool
blah = flip evalStateT [] $ flip evalStateT 1 $ flip evalStateT True $ do
  inc
  liftIO . print =<< toggle
  toggle
  inc
  toggle
  liftIO $ print 3
  toggle
