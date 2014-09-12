{-# LANGUAGE
  TemplateHaskell, RankNTypes, ConstraintKinds #-}

module Command where


import           ClassyPrelude hiding (try, putStrLn, getLine)
import           Data.List (findIndex)
import           Prelude ((!!))
import qualified Data.Map.Strict as M
import           Control.Applicative (many)
import           Text.Parsec hiding ((<|>), many, setInput)
import           Text.Parsec.Text
import           Text.Printf
import           Data.Char (isSpace)
import           Data.Traversable (sequenceA)
import           System.Random hiding (split)
import           Control.Monad (zipWithM)
import           Data.Time.LocalTime

import           GUI
import           DB
import           Helpers
import           Patterns
import           Parse
import           Paths
import           Notes
import           IPC
import           N
import           ClassesLens hiding (cons, noneOf)


type M' m = (MonadGUI m, N' m, IO' m)
type M a = M' m => m a


execute :: Text → M ()
execute = either (liftIO' . print) evalCommand . parse commandP ""


showNoteTreesC :: MPath → M ()
showNoteTreesC p = do
  notes ← lookupMPath p
  case notes of
    [] → putStrLn "no notes matching the pattern"
    ns → do w ← getTerminalWidth
            putStrLn ""
            forM_ ns $ \n → 
              putStrLn . unlines . indent 2 /$/ showNoteTree (w-2) Nothing n

listSubsC :: Path → M ()
listSubsC p = do
  mbNote ← lookupPath p
  case mbNote of
    Nothing → putStrLn "couldn't find note"
    Just n  → do w  ← getTerminalWidth
                 ss ← use $ atNid n.subs
                 putStrLn ""
                 putStrLn . unlines . indent 2 /$/ showNoteList (w-2) ss 

randC :: Path → M ()
randC p = do
  mbNote ← lookupPath p
  case mbNote of
    Nothing → putStrLn "couldn't find note"
    Just n  → do w  ← getTerminalWidth
                 ss ← use (atNid n.subs)
                 i  ← liftIO' $ randomRIO (0, length ss - 1)
                 t  ← use $ atNid (ss !! i) . title
                 putStrLn $ unlines $ ("":) $ indent 2 $ 
                   showNoteTitle (w-2) Nothing t

addC :: Path → Text → M ()
addC p s = do
  mbNote ← lookupPath p
  case mbNote of
    Nothing   → putStrLn "path doesn't exist"
    Just note → commit ("add " ++ showPath p) $ do
                  e ← newNote [] s
                  addSub e note
                  saveDB

plusC :: Path → Text → M ()
plusC p s = do
  mbNote ← lookupPath p
  case mbNote of
    Nothing   → putStrLn "path doesn't exist"
    Just note → case s of
      ""   → commit ("plus " ++ showPath p) (addLoop [note])
      line → commit ("plus " ++ showPath p) $ do
               child ← newNote [] line
               addSub child note
               saveDB
  where
    -- The list argument contains NIDs of notes along the path in the
    -- tree. The first one is the deepest one.
    addLoop :: [NID] → M ()
    addLoop   []   = saveDB
    addLoop (n:ns) = do
      titles ← forM (n:ns) $ \n → use (atNid n.title)
      -- @'intercalate "\n"@ since 'unlines' adds newline after last line,
      -- which is wrong.
      let prompt = intercalate "\n" $
            zipWith (\i t → times i "> " ++ t)
                    [1..] (reverse titles ++ [""]) 
      line ← getLine prompt
      if | null line    → addLoop ns
         | ">" ^== line → goDeeper (tailDef line)
         | otherwise    → do
             child ← newNote [] line
             addSub child n
             savePartial
             addLoop (n:ns)
      where
        goDeeper :: Text → M ()
        goDeeper line = do
          ss ← use $ atNid n.subs
          if | null ss          → 
                 putStrLn "nothing to add to!" >> addLoop (n:ns)
             | all isSpace line → 
                 addLoop ((lastEx ss):n:ns)
             | " " ^== line     → do
                 child ← newNote [] (tailDef line)
                 addSub child (lastEx ss)
                 savePartial
                 addLoop (n:ns)
             | otherwise        → do
                 putStrLn "did you forget a space?"
                 addLoop (n:ns)

data Renumber = 
  Swap Int | 
  Insert Int | 
  MoveToTop | MoveToBottom |
  MoveUp Int | MoveDown Int

renumberC :: Path → Renumber → M ()
renumberC [] _ = putStrLn "can't renumber root"
renumberC  p r = do
  mbParent ← sequenceA . fmap (use . atNid) /$/ lookupPath (initDef p)
  mbIndex  ← case mbParent of
    Nothing   → return Nothing
    Just note → select (lastEx p) $ note ^. subs
  case (mbParent, mbIndex) of
    (Nothing, _) → putStrLn "parent doesn't exist"
    (_, Nothing) → putStrLn "note doesn't exist"
    (Just parent, Just i) → do 
      let ns = parent ^. subs
          doInsert :: Text → Int → M ()
          doInsert s n 
            | n < 1         = putStrLn "destination is too low"
            | n > length ns = putStrLn "destination is too high"
            | otherwise     = 
                commit ("renumber/" ++ s ++ " " ++ showPath p) $ do
                  atNid (parent ^. nid) . subs .= insertFromTo i (n-1) ns
                  saveDB
          doSwap :: Int → M ()
          doSwap n 
            | n < 1         = putStrLn "destination is too low"
            | n > length ns = putStrLn "destination is too high"
            | otherwise     = 
                commit ("renumber/swap" ++ showPath p) $ do
                  atNid (parent ^. nid) . subs .= swapList i (n-1) ns
                  saveDB
      case r of
        Swap n       → doSwap n
        Insert n     → doInsert "insert" n
        MoveToTop    → doInsert "top" 1
        MoveToBottom → doInsert "bottom" (length ns)
        MoveUp n     → doInsert "up" (i-n+1)
        MoveDown n   → doInsert "down" (i+n+1)

moveNoteC :: MPath → Path → M ()
moveNoteC ap bp = do
  mbOrig ← lookupMPath ap
  mbDest ← lookupPath bp
  case (mbOrig, mbDest) of
    ([],   _   ) → putStrLn "origin doesn't exist"
    (_, Nothing) → putStrLn "destination doesn't exist"    
    (orig, Just dest) → 
      commit ("move " ++ showMPath ap ++ " to " ++ showPath bp) $ do
        removeNoteC ap
        mapM_ (`addSub` dest) orig
        saveDB


select :: Sel → [NID] → DB (Maybe Int)
select sel nids = index sel $/ mapM (use . atNid) nids
  where
    index (SelIndex i) ns 
      | i < 1 || i > length ns = Nothing
      | otherwise              = Just (i-1)
    index (SelTitle t) ns = findIndex (matchTitle t . (^. title)) ns 


selectNid :: Sel → [NID] → DB (Maybe NID)
selectNid sel nids = fmap (nids !!) $/ select sel nids


mselect :: MSel → [NID] → DB [Int]
mselect (MSelList sels) nids = catMaybes $/ mapM (`select` nids) sels


mselectNids :: MSel → [NID] → DB [NID]
mselectNids msel nids = map (nids !!) $/ mselect msel nids


lookupPath :: Path → DB (Maybe NID)
lookupPath ps = go ps =<< use root
  where
    go   []   ni = return (Just ni)
    go (p:ps) ni = do
      note   ← use $ atNid ni
      mbNid' ← selectNid p (note ^. subs)
      case mbNid' of
        Nothing  → return Nothing
        Just ni' → go ps ni'


lookupMPath :: MPath → DB [NID]
lookupMPath ps = go ps =<< use root
  where
    go   []   ni = return [ni]
    go (p:ps) ni = do
      note ← use $ atNid ni
      nids ← mselectNids p (note ^. subs)
      concatMapM (go ps) nids


-- | Purges notes which aren't accessible anymore from the database.
gcNotes :: DB ()
gcNotes = do
  rt ← use root
  nids ← allNids rt
  notes %= M.filterWithKey (\k _ → k `elem` (rt:nids))
  where
    allNids n = (n:) $/ concatMapM allNids /$/ use (notes.ix n.subs)


showNoteTreeC :: Path → M ()
showNoteTreeC p = do
  mbNote ← lookupPath p
  case mbNote of
    Nothing   → putStrLn "couldn't find note"
    Just note → do w ← getTerminalWidth
                   putStrLn ""
                   putStrLn . unlines . indent 2 /$/ 
                     showNoteTree (w-2) Nothing note


-- TODO: deletes from *all* subs. This is *wrong*.
removeNoteC :: MPath → M ()
removeNoteC p = do
  notes ← lookupMPath p
  case notes of
    [] → putStrLn "notes not found"
    _  → commit ("delete " ++ showMPath p) $ do
           deleteEverywhere (`elem` notes)
           saveDB

editNoteC :: Path → Maybe Text → M ()
editNoteC p mbs = do
  mbNote ← lookupPath p
  case mbNote of
    Nothing → putStrLn "note not found"
    Just ni → do
        note ← use $ atNid ni
        s' ← case mbs of
               Nothing → do
                 setInput (note ^. title, "")
                 getLine ("editing " ++ showPath p ++ " >")
               Just s  → return s
        if null s' 
          then putStrLn "no empty notes!"
          else commit ("edit " ++ showPath p) $ do
                 timeStamp ← liftIO' getZonedTime
                 zoom (atNid ni) $ do
                   title        .= s'
                   lastModified .= timeStamp
                 saveDB

undoC :: M ()
undoC = do
  mbOp ← undo
  case mbOp of
    Nothing → putStrLn "Nothing to undo."
    Just op → do
      putStrLn $ "Undone “" ++ op ++ "”."
      saveDB

redoC :: M ()
redoC = do
  mbOp ← redo
  case mbOp of
    Nothing → putStrLn "Nothing to redo."
    Just op → do
      putStrLn $ "Redone “" ++ op ++ "”."
      saveDB

gcNotesC :: M ()
gcNotesC = do
  saveDB
  oldSize ← length $/ use notes
  commit "garbage collection" $ do
    gcNotes
    saveDB
  newSize ← length $/ use notes
  putStrLn $ pack $
    printf "removed %d entries out of %d" (oldSize - newSize) oldSize

clearC :: M ()
clearC = do
  freshN
  saveDB

exitC :: M ()
exitC = liftIO' mainExitSuccess


-- helper parsers

word :: Text → Parser Text
word t = try (text t <* (spaces1 <|> eof)) <?> unpack t

--trailingSpaces = spaces <* eof


-- argument parsers

stringP :: Parser Text
stringP = pack $/ between (char '"') (char '"') 
                    (many $ escaped <|> noneOf "\"")
  where
    escaped = string "\\" >> choice (zipWith escapedChar cs rs)
    escapedChar c r = char c *> pure r
    cs = ['\\', '\"']
    rs = ['\\', '\"']

-- command parsers

-- | Parses any of aliases for the command, using 'word'; returns the 1st
-- alias in the list.
commandName :: [Text] → Parser Text
commandName ts@(t:_) = choice (map word ts) *> pure t
commandName []       = error "Command.commandName: empty list of commands"


evalCommand :: Command → M ()
evalCommand ("clear", []) =
  clearC
evalCommand ("exit", []) =
  exitC
evalCommand ("+", [APath p, AText c]) =
  plusC p c
evalCommand ("+", [APath p]) =
  plusC p ""
evalCommand ("add", [APath p, AText c]) =
  addC p c
evalCommand ("list", [APath p]) =
  listSubsC p
evalCommand ("list", []) =
  listSubsC currentNote
evalCommand ("tree", [APath p]) =
  showNoteTreeC p
evalCommand ("tree", []) =
  showNoteTreeC currentNote
evalCommand ("random", [APath p]) =
  randC p
evalCommand ("remove", [AMPath p]) =
  removeNoteC p
evalCommand ("undo", []) =
  undoC
evalCommand ("redo", []) =
  redoC
evalCommand ("move", [AMPath f, APath t]) =
  moveNoteC f t
evalCommand ("insert", [APath p, ANat n]) =
  renumberC p (Insert (fromInteger n))
evalCommand ("swap", [APath p, ANat n]) =
  renumberC p (Swap (fromInteger n))
evalCommand ("top", [APath p]) =
  renumberC p MoveToTop
evalCommand ("bottom", [APath p]) =
  renumberC p MoveToBottom
evalCommand ("up", [APath p, ANat n]) =
  renumberC p (MoveUp (fromInteger n))
evalCommand ("up", [APath p]) =
  renumberC p (MoveUp 1)
evalCommand ("down", [APath p, ANat n]) =
  renumberC p (MoveDown (fromInteger n))
evalCommand ("down", [APath p]) =
  renumberC p (MoveDown 1)
evalCommand ("edit", [APath p, AText t]) =
  editNoteC p (Just t)
evalCommand ("edit", [APath p]) =
  editNoteC p Nothing
evalCommand ("gc", []) =
  gcNotesC
evalCommand c =
  putStrLn $ "Command.evalCommand: unknown command: " ++ tshow c

data Arg = AText Text | APath Path | ANat Integer | AMPath MPath
  deriving (Show)

type Command = (Text, [Arg])

commandP :: Parser Command
commandP = choice [
  treeP, listP, randP,
  clearP, exitP, gcP,
  undoP, redoP,
  plusP, addP, removeP, editP,
  moveP,
  insertP, swapP, topP, bottomP, upP, downP ]

clearP :: Parser Command
clearP = (,) <$> commandName ["clear"] <*> pure []

exitP :: Parser Command
exitP = do
  (,) <$> commandName ["exit", "quit", "ex", "q"] <*> pure []

plusP :: Parser Command
plusP = do
  string "+"
  lonelyPlus ← tryCheck space
  path ← if lonelyPlus then return currentNote else pathP
  spaces1 <|> eof
  content ← consumeRest
  return ("+", [APath path, AText content])

addP :: Parser Command
addP = (,)
  <$> commandName ["add"]
  <*> sequence [APath $/ pathP <* spaces1,
                AText $/ stringP ]

listP :: Parser Command
listP = do
  c ← commandName ["list", "ls"]
  mbP ← optionMaybe pathP
  return $ case mbP of
    Nothing → (c, [])
    Just p  → (c, [APath p])

treeP :: Parser Command
treeP = do
  c ← commandName ["tree", "tr"]
  mbP ← optionMaybe pathP
  return (c, catMaybes [APath $/ mbP])

randP :: Parser Command
randP = (,)
  <$> commandName ["random", "rand"]
  <*> sequence [APath $/ pathP]

removeP :: Parser Command
removeP = (,)
  <$> commandName ["remove", "delete", "rm", "del"]
  <*> sequence [AMPath $/ mpathP]

undoP :: Parser Command
undoP = (,) <$> commandName ["undo"] <*> pure []

redoP :: Parser Command
redoP = (,) <$> commandName ["redo"] <*> pure []

moveP :: Parser Command
moveP = (,)
  <$> commandName ["move", "mv"]
  <*> sequence [AMPath $/ mpathP,
                APath $/ spaces1 *> pathP ]

insertP :: Parser Command
insertP = (,)
  <$> commandName ["insert", "ins"]
  <*> sequence [APath $/ pathP,
                ANat $/ spaces1 *> natural ]

swapP :: Parser Command
swapP = (,)
  <$> commandName ["swap"]
  <*> sequence [APath $/ pathP,
                ANat $/ spaces1 *> natural ]

topP :: Parser Command
topP = (,)
  <$> commandName ["top"]
  <*> sequence [APath $/ pathP]

bottomP :: Parser Command
bottomP = (,)
  <$> commandName ["bottom", "bot"]
  <*> sequence [APath $/ pathP]

upP :: Parser Command
upP = do
  c ← commandName ["up"]
  p ← pathP
  mbN ← optionMaybe (spaces1 *> natural)
  return (c, catMaybes [Just (APath p), ANat $/ mbN])

downP :: Parser Command
downP = do
  c ← commandName ["down", "dn"]
  p ← pathP
  mbN ← optionMaybe (spaces1 *> natural)
  return (c, catMaybes [Just (APath p), ANat $/ mbN])

editP :: Parser Command
editP = do
  c ← commandName ["edit", "ed"]
  p ← pathP
  mbT ← optionMaybe (spaces1 *> consumeRest)
  return (c, catMaybes [Just (APath p), AText $/ mbT])

gcP :: Parser Command
gcP = (,) <$> commandName ["gc"] <*> pure []


-- | Loads the DB: checks whether DB already exists, tries to create it if it
-- doesn't, and backups it if it exists but can't be parsed.
loadDB :: M ()
loadDB = do
  exists ← liftIO' . existsDB /$/ use dbLocation
  unless exists $ freshN >> saveDB
  mbDb ← readDB /$/ use dbLocation
  case mbDb of
    Just d  → do
      freshN 
      db .= d
    Nothing → do
      putStrLn "database is corrupted, backing it up"
      backupDB /$/ use dbLocation
      freshN
      saveDB


-- | Saves the DB.
saveDB :: M ()
saveDB = join $ writeDB <$> use dbLocation <*> use db


-- | Supposed to be used in the middle of some database-modifying operation
-- (say, interactively adding multiple notes), when we don't want to lose
-- user input. Currently doesn't differ from 'saveDB'.
savePartial :: M ()
savePartial = saveDB


{-

import ClassyPrelude

import Helpers

data Type = TPath | TMPath | TText

data Command = Command {
  _commandId   :: Text,
  _commandArgs :: [Type],
  _commandRes  :: Type }

makeLenses ''Command

data Expr =
  EPath Path |
  EMPath MPath |
  EText Text |
  EApply Command [Expr]
  deriving (Show)

-- this function has to analyze the command the user is typing, along
-- with the position of cursor in it, and produce some (hopefully)
-- useful hints
analyze :: Text → Int → M Text
analyze c i = return $ pack $
  printf "This is command %s, and the cursor is at %d."
         (unpack c) i


-}
