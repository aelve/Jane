{-# LANGUAGE
  ConstraintKinds, TemplateHaskell #-}

module Notes (
  NID, Note (..), NStore,
  runDB, DBState (..), DB, DB', DBT,
  nid, subs, title, created, lastModified,
  db, notes, root,
  freshDB,
  genNid,
  newNote, atNid, addSub, shuffleSubs, deleteEverywhere )
  where


import           ClassyPrelude hiding (getLine, putStrLn, on)
import           Text.Printf
import           Data.UUID
import           Control.Monad.Classes
import           Control.Monad.State (StateT, evalStateT)
import           System.Random (randomIO)
import           System.Random.Shuffle (shuffleM)
import           Data.Time.LocalTime

import           ClassesLens
import           Helpers


-- | Unique note identifier.
type NID = UUID


-- | A single note.
data Note = Note {
  -- | Note identifier.
  _nid          :: NID,
  -- | A list of note's children.
  _subs         :: [NID],
  -- | Note's title.
  _title        :: Text,
  -- | Note's time of creation.
  _created      :: ZonedTime,
  -- | Note's time of modification.
  _lastModified :: ZonedTime }
  deriving (Read, Show)

makeLenses ''Note


-- | A structure which binds 'Note's to their 'NID's.
type NStore = Map NID Note


-- | Permanently stored information about notes (in other words, what goes
-- into the database file).
data DBState = DBState {
  _notes :: NStore,
  _root  :: NID }
  deriving (Read, Show)

makeLenses ''DBState


-- | Monads with this constraint provide access to the note database ('DB').
type DB' m = (Functor m, MonadState DBState m)


-- | A type for actions with the access to the note database. See 'DB''.
type DB a = DB' m => m a


-- | A 'DB' monad transformer.
type DBT = StateT DBState


-- | Creates a fresh database and runs a 'DB' action.
runDB :: IO' m => DBT m a → m a
runDB act = evalStateT (freshDB >> act) err
  where
    err = error "Notes.runDB: tried to access undefined state."


-- | A 'Lens' for accessing 'DBState'.
db :: Lens' DBState DBState
db = id


-- | Generates a fresh 'DBState' with an untitled root note and no children.
freshDB :: (IO' m, DB' m) => m ()
freshDB = do
  db .= DBState {
    _notes = mempty, 
    _root  = error "Notes.freshDB: ?!" }
  rt ← newNote [] "/"
  root .= rt


-- | Generates a random NID.
genNid :: IO_ NID
genNid = liftIO' randomIO


-- | Adds a note to the database (without yet linking it anywhere).
newNote ::
  (IO' m, DB' m) =>
  [NID] →            -- ^ Note's children.
  Text →             -- ^ Note's title.
  m NID              -- ^ Created note's 'NID'.
newNote s t = do
  nid ← genNid
  timeStamp ← liftIO' getZonedTime
  let n = Note {
        _nid          = nid, 
        _subs         = s,
        _title        = t,
        _created      = timeStamp,
        _lastModified = timeStamp }
  notes.at nid .= Just n
  return nid


-- | Makes a note a child of another note (without checking if it was already
-- a child!).
addSub ::
  NID →     -- ^ Child note.
  NID →     -- ^ Parent note.
  DB ()
addSub note parent = atNid parent.subs <>= [note]


-- | A lens for accessing a note, given its 'NID'.
-- 
-- Throws an exception if note is not found.
atNid :: NID → Lens' DBState Note
atNid n = lens get set
  where
    err = error $ printf "atNid: %s not found" (show n) 
    get ms = fromMaybe err $ ms ^. notes.at n
    set ms note | ms ^. notes.to (member n) = ms & notes.ix n .~ note
                | otherwise                 = err


-- | Randomly shuffles children of a note.
shuffleSubs :: (DB' m, IO' m) => NID → m ()
shuffleSubs note = do
  subs' ← liftIO' . shuffleM /$/ use $ atNid note.subs
  atNid note.subs .= subs'


-- | Traverses the entire tree, removing note with NIDs satisfying the
-- predicate from every other note's list of children.
deleteEverywhere :: (NID → Bool) → DB ()
deleteEverywhere p = go =<< use root
  where
    go n = do
      ss ← use (notes.ix n.subs)
      let ss' = filter (not . p) ss
      mapM_ go ss'
      (notes.ix n.subs) .= ss'
