{-# LANGUAGE FunctionalDependencies, TypeFamilies, UndecidableInstances #-}


-- | This module reexports all functionality of 'Control.Lens', replacing
-- 'mtl'-specific combinators (e.g. '.=', 'use') with
-- 'monad-classes'-specific ones.


module ClassesLens (
  module Control.Lens,
  use, view, (.=), (%=), (<>=), zoom )
  where


import Prelude
import Data.Monoid
import Control.Monad (liftM)
import Control.Monad.Classes
import Data.Profunctor.Unsafe
import Control.Lens hiding (use, view, (.=), (%=), (<>=), zoom)
import qualified Control.Monad.Trans.State.Lazy as Lazy


infix 4 .=, <>=, %=
infixr 2 `zoom`


asks :: MonadReader r m => (r → a) → m a
asks f = liftM f ask

use :: MonadState s m => Getting a s a → m a
use l = gets (^. l)
{-# INLINE use #-}

view :: MonadReader s m => Getting a s a → m a
view l = asks (getConst #. l Const)
{-# INLINE view #-}

(.=) :: MonadState s m => ASetter s s a b → b → m ()
l .= b = modify (l .~ b)
{-# INLINE (.=) #-}

(%=) :: (Profunctor p, MonadState s m) => Setting p s s a b → p a b → m ()
l %= f = modify (l %~ f)
{-# INLINE (%=) #-}

(<>=) :: (MonadState s m, Monoid a) => ASetter' s a → a → m ()
l <>= a = modify (l <>~ a)
{-# INLINE (<>=) #-}


-- | This 'zoom' is cruelly simplistic and only works in primitive cases.
zoom :: (MonadState s m) =>
        Lens' s t → Lazy.State t a -> m a
zoom l (Lazy.StateT t) = do
  s ← get
  let (a, s') = l (runIdentity . t) s
  put s'
  return a
