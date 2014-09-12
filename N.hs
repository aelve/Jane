{-# LANGUAGE
  ConstraintKinds, TemplateHaskell #-}

module N (
  runAux, TAux, N, N',
  freshN,
  AuxState, prevStates, undoneStates, prevOps, undoneOps, dbLocation,
  undo, redo, commit,
  emptyAuxState )
  where


import           ClassyPrelude
import           Control.Monad.State (StateT, evalStateT)
import           Control.Monad.Classes

import           ClassesLens
import           DB
import           Notes
import           Helpers


-- | Auxiliary program state regarding notes.
data AuxState = AuxState {
  -- | Previous states of the database.
  _prevStates   :: [DBState],
  -- | States of the database that were undone.
  _undoneStates :: [DBState],  
  -- | Descriptions of operations performed on previous states. For instance,
  -- current state was produced from @'head' 'prevStates'@ by applying
  -- @'head' 'prevOps'@.
  _prevOps      :: [Text],
  -- | Descriptions of operations performed on undone states. @'head'
  -- 'undoneStates'@ = current state + @'head' 'undoneOps'@.
  _undoneOps    :: [Text],
  -- | The location of the database.
  _dbLocation   :: DBLocation }
  deriving (Show)

makeLenses ''AuxState


-- | A 'Lens' for 'AuxState'.
auxState :: Lens' AuxState AuxState
auxState = id


-- | An 'Aux' monad transformer.
type TAux = StateT AuxState


-- | Monads with this constraint provide access to the additional state
-- concerning notes ('AuxState'), as well as the note database.
-- 
-- @
--     type N' m = ('DB'' m, 'MonadState' 'AuxState' m)
-- @
type N' m = (Functor m, DB' m, MonadState AuxState m)


-- | A type for actions with the access to the note database and the
-- additional state. See 'N''.
type N a = N' m => m a


-- | Runs an 'Aux' action.
runAux :: Monad m => AuxState → TAux m a → m a
runAux = flip evalStateT


-- | Records DB state before and after the operation and handles undoing.
commit ::
  (N' m) =>
  Text →    -- ^ Description of operation being performed.
  m () →
  m ()
commit op act = do
  old ← use db
  act
  let annotate s = "undo \"" ++ s ++ "\""
      -- mirror [1,2,3] == [1,2,3,2,1]
      mirror s   = s ++ drop 1 (reverse s)
  unStates      ← use undoneStates
  unOps         ← use undoneOps
  prevStates   %= (++) (mirror (old : unStates))
  undoneStates .= []
  prevOps      %= (++) (op : map annotate unOps ++ reverse unOps)
  undoneOps    .= []
  return ()


-- | Undoes the last operation. Returns 'Nothing' if there wasn't anything to
-- undo, or @'Just' operationName@.
undo :: N (Maybe Text)
undo = do
  p ← use prevStates
  case p of
    []                 → return Nothing
    (prState:prStates) → do
      old   ← use db
      prOps ← use prevOps

      db           .= prState
      prevStates   .= prStates
      undoneStates %= (old:)
      prevOps      .= tailEx prOps
      undoneOps    %= (headEx prOps:)

      return (Just $ headEx prOps)


-- | Redoes the last undone operation. Returns 'Nothing' if there wasn't
-- anything to redo, or @'Just' operationName@.
redo :: N (Maybe Text)
redo = do
  u ← use undoneStates
  case u of
    []                 → return Nothing
    (unState:unStates) → do
      old   ← use db
      unOps ← use undoneOps
 
      db           .= unState
      prevStates   %= (old:)
      undoneStates .= unStates
      prevOps      %= (headEx unOps:)
      undoneOps    .= tailEx unOps

      return (Just $ headEx unOps)


-- | An empty 'AuxState'. By default the database isn't stored anywhere
-- ('NoDB').
emptyAuxState :: AuxState
emptyAuxState = AuxState {
  _prevStates   = [], 
  _undoneStates = [],
  _prevOps      = [],
  _undoneOps    = [],
  _dbLocation   = NoDB }


-- | Sets some initial value for the database and auxiliary state.
-- Specifically, empty note tree with a single root node and no undo
-- information. 'dbLocation' is preserved.
-- 
-- The 'IO'' constraint is needed to generate a random root note.
-- TODO: get rid of this constraint?
freshN :: (IO' m, N' m) => m ()
freshN = do
  dbL ← use dbLocation
  auxState .= emptyAuxState
  dbLocation .= dbL
  freshDB
