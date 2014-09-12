{-# LANGUAGE
  TemplateHaskell, ConstraintKinds, DataKinds, TypeFamilies #-}


module GUI.GTK (
  GTK', GTK, GTKT, runGTK, GUI.MonadGUI(..),
  popupWindow )
  where


import           ClassyPrelude hiding (getLine, putStrLn, on, Builder)
import qualified ClassyPrelude as P
import           Control.Monad.Classes
import           Control.Monad.Reader (Reader, ReaderT, runReaderT)
import           Control.Concurrent.Lifted (forkOS)
import           Control.Monad.Loops (iterateUntil)
import           System.Random (randomIO)
import           Text.Printf
import           Control.Concurrent.Chan
import           Control.Concurrent.STM.TChan
import           Control.Concurrent (threadDelay)
import           Graphics.UI.Gtk hiding (get, set)
import qualified Graphics.UI.Gtk as G (get, set)
import           Graphics.UI.Gtk.Builder
import           Graphics.UI.Gtk.WebKit.WebView
import           Graphics.UI.Gtk.WebKit.WebInspector
import           Graphics.UI.Gtk.WebKit.WebSettings
import           Graphics.UI.Gtk.WebKit.DOM.Document
import           Graphics.UI.Gtk.WebKit.DOM.Node
import           Data.Text (splitOn)
import           Data.Char (isSpace)

import qualified GUI as GUI
import           GUI (Output(..))
import           Helpers
import           Files
import           ClassesLens


-- | A queue of lines. It has to be a 'TChan' because we need 'tryReadTChan'
-- for 'getLine'.
type LQueue = TChan Text


-- | Internal state of GUI-handling code.
data GTKState = GTKState {
  _inputQueue :: LQueue,       -- ^ Queue holding all lines entered so far.
  _wMain      :: Window,       -- ^ Main window.
  _wCommand   :: TextView,     -- ^ Command input entrybox.
  _wPrompt    :: Label,        -- ^ Prompt label.
  _wOutput    :: TextView,     -- ^ Main output box.
  _wOutput2   :: WebView,      -- ^ WebKit-based output.
  _hasQuit    :: IORef Bool }  -- ^ See 'finishGTK'.

makeLenses ''GTKState


-- We need GTK' and GTK'' separated to avoid code duplication (see the
-- signature of runGTK).
type GTK'' m = IO' m


-- | Monads under this constraint have to provide access to 'IO' and be
-- instances of @'MonadReader' 'GTKState'@.
type GTK' m = (GTK'' m, MonadReader GTKState m)


-- | A type for actions with access to the GUI. See 'GTK''.
type GTK a = GTK' m => m a


-- | A 'GTK' monad transformer.
type GTKT = ReaderT GTKState


instance GTK'' m => GUI.MonadGUI (GTKT m) where
  getLine   = getLine
  getLine'  = getLine'
  setPrompt = setPrompt
  output    = output
  setTitle  = setTitle
  setInput  = setInput
  finishGUI = finishGTK


-- | Prepares the main window.
buildMain :: Builder
           → IORef Bool
           → IO Window
buildMain builder hasQuitVar = do
  main ← builderGetObject builder castToWindow "main"

  -- When main window is destroyed, we quit GTK event loop and set 'hasQuit'
  -- flag (see 'finishGTK).
  main `after` objectDestroy $ do
    mainQuit
    writeIORef hasQuitVar True
    mainExitSuccess
  return main


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
buildCommand :: Builder → LQueue → IO TextView
buildCommand builder lQueue = do
  command ← builderGetObject builder castToTextView "command"
  buffer  ← G.get command textViewBuffer

  sigInsertIdRef ← newIORef
    (error "GUI.GTK.buildCommand: IORef didn't get written.")
  sigInsertId ← buffer `on` bufferInsertText $
    onInsert buffer sigInsertIdRef
  writeIORef sigInsertIdRef sigInsertId

  return command

  where
    -- Add a string to the queue.
    addLine = atomically . writeTChan lQueue . pack

    onInsert buffer sigIdRef iter str = do
      sigId ← readIORef sigIdRef
      text ← G.get buffer textBufferText
      -- Temporarily block *this* handler from running again when inserting
      -- text (because actual insertion is done by the default handler, which
      -- fires after this one).
      signalBlock sigId
      
      -- If the buffer was empty and we're inserting multiple lines, they are
      -- interpreted as many commands.
      if | null text && '\n' `elem` str →
             mapM_ addLine (lines str)
      -- If the buffer wasn't empty and we're inserting a single newline, it
      -- means that Enter has been pressed and the whole line must be
      -- commited to the input queue.
         | str == "\n" → do
             addLine text
             G.set buffer [textBufferText := ""]
      -- Otherwise, we simply concatenate inserted lines (if there were many)
      -- and insert them.
         | otherwise →
             textBufferInsert buffer iter (unwords (lines str))

      -- Unblock this handler.
      signalUnblock sigId
      -- Stop this signal from going further and firing the default handler.
      signalStopEmission buffer "insert-text"


-- | Prepares the label widget which displays hints to the user.
buildPrompt :: Builder → IO Label
buildPrompt builder = do
  builderGetObject builder castToLabel "prompt"


-- | Prepares the main output widget. 
buildOutput :: Builder → IO TextView
buildOutput builder = do
  output ← builderGetObject builder castToTextView "output"
  -- We want monospace font for data output ('cause of indentation).
  monoFont ← fontDescriptionNew
  fontDescriptionSetFamily monoFont "Monospace"
  widgetModifyFont output (Just monoFont)
  return output


buildOutput2 :: Builder → IO WebView
buildOutput2 builder = do
  outputWindow ← builderGetObject builder castToScrolledWindow "outputWindow"
  webView ← webViewNew
  G.set outputWindow [containerChild := webView]

  -- Enable WebKit's “Inspect” functionality.
  webSettings ← G.get webView webViewWebSettings
  G.set webSettings [webSettingsEnableDeveloperExtras := True]
  inspector ← G.get webView webViewInspector
  inspector `on` inspectWebView $ \_ → do
    iWindow  ← windowNew
    iWebView ← webViewNew
    G.set iWindow [containerChild := iWebView]
    widgetShowAll iWindow
    return iWebView

  return webView


-- | Creates GTK-based GUI for Jane and executes some code.
runGTK :: GTK'' m => GTKT m a → m a
runGTK gui = do

  -- Create an MVar which would hold the GTKState we're going to create now.
  stateVar ← liftIO' newEmptyMVar

  -- And an IORef for the 'hasQuit' flag.
  hasQuitVar ← newIORef False

  -- We want to run the GTK event handling loop in a separate thread (well,
  -- because it's a *loop*).
  liftIO' $ forkOS $ do
    
    -- Initialize GTK.
    initGUI

    -- Create a channel for input lines.
    lQueue ← newTChanIO

    -- Load GUI description from Glade file. If “main.glade” exists in
    -- current directory, it's preferred over the installed file.
    builder ← builderNew
    builderAddFromFile builder /$/ fpToString $/ getDataFileName "main.glade"

    -- Create all widgets, assigning them necessary properties, event
    -- handlers and so on.
    command ← buildCommand builder lQueue
    prompt  ← buildPrompt  builder
    output  ← buildOutput  builder
    output2 ← buildOutput2 builder
    main    ← buildMain    builder hasQuitVar

    -- Show main window.
    widgetShowAll main

    -- Build the inner state and share it to the outside.
    let state = GTKState {
          _inputQueue = lQueue,
          _wMain      = main,
          _wCommand   = command,
          _wPrompt    = prompt,
          _wOutput    = output,
          _wOutput2   = output2,
          _hasQuit    = hasQuitVar } 
    putMVar stateVar state

    -- Start GTK event handling loop.
    mainGUI

  state ← liftIO' $ takeMVar stateVar

  runReaderT gui state


-- | Lifts a GTK-related action.
-- 
-- Mere 'liftIO' won't do here, since when we're not running from the main
-- thread we have to wrap all GTK-related business with 'postGUISync'.
liftGTK :: IO a → GTK a
liftGTK = liftIO' . postGUISync


-- | Sets text of the prompt label.
setPrompt :: Text → GTK ()
setPrompt s = do
  prompt ← view wPrompt
  liftGTK $ do
    G.set prompt [labelText := unpack s]


-- | Closes the program. Doesn't save any databases or close any handles,
-- just destroys the window.
-- 
-- Here's how quitting happens:
-- 
--   * When main window is closed, the loop is terminated and 'hasQuit' flag
--   is set.
-- 
--   * When 'finishGUI' is called, the main window is destroyed and the GTK
--   loop is terminated – but only if 'hasQuit' flag isn't set, because
--   destroying the main window twice leads to Bad Things (some threads hang,
--   etc.) and the code in Main doesn't care to distinguish between
--   quitting-because-window-was-closed and quitting-programmatically.
finishGTK :: GTK ()
finishGTK = unlessM (readIORef /$/ view hasQuit) $ do
  main ← view wMain
  flip writeIORef True /$/ view hasQuit
  liftGTK $ widgetDestroy main
  liftIO' mainQuit


output :: Output → GTK ()
output = undefined


-- | A box is simply a piece of width-dependent text.
type TBox = Reader Int [Text]


-- | Creates a 'TBox' from some text.
box :: Text → TBox
box t = do
  width ← ask
  return (wordWrap width t)


-- | Puts some text to the left of the box.
-- 
-- @
-- "1. " `stackLeft` box "I'm fine thanks and how are you"
-- 
-- 1. I'm fine thanks and
--    how are you
-- @
stackLeft :: Text → TBox → TBox
stackLeft t b = do
  ls ← local (subtract (length t)) b
  case ls of
    [] → [t]
    (x:xs) → (t ++ x) : map (replicate (length t) ' ' ++) xs


-- | Fits some text to given width.
wordWrap :: Int → Text → [Text]
wordWrap   1   _ = error "wordWrap: width = 1, seriously?"
wordWrap width t = go width "" (separate (not . isSpace) t)
  where
    -- TODO: check if `go` is quadratic in respect to width!
    go w s        []      = if null s then [] else [s]
    go w s ((Right t):ts)
      -- the word fits, great
      | length t <= w     = go (w-length t) (s ++ t) ts
      -- the word would've fitted, just not on this line
      | length t <= width = s : go width "" ((Right t):ts)
      -- there's one cell left (or less), we can't even hyphenate now
      | w <= 1            = s : go width "" ((Right t):ts)
      -- we can (and will) hyphenate
      | otherwise         = (s ++ take (w-1) t ++ "-") : 
                             go width "" ((Right (drop (w-1) t)):ts)
    go w s ((Left t):ts)
      -- spaces fit
      | length t <= w   = go (w-length t) (s ++ t) ts
      -- one space doesn't fit (sucker! but ne is forgiven)
      | length t == w-1 = (s ++ take w t) : go width "" ts
      -- we strip one space and move to new line
      | otherwise       = (s ++ take w t) : 
                          go width "" ((Left (drop (w+1) t)):ts)

showNoteTitle :: Int
               → Maybe Int
               → Text
               → [Text]
showNoteTitle width mbI title | width < 8 = showNoteTitle 8 mbI title
showNoteTitle width mbI title = case mbI of
  Nothing → wordWrap width title
  Just i  → let n = tshow i ++ ") "
                t = wordWrap (width - length n) title
            in  (n ++ headEx t) : indent (length n) (tailDef t)


showNoteTree :: Int        -- ^ width
              → Maybe Int  -- ^ Nothing = unnumbered, Just i = note #i
              → NID        -- ^ note to show
              → M [Text]
showNoteTree width mbI nid | width < 8 = showNoteTree 8 mbI nid
showNoteTree width mbI nid = do
  note   ← use $ atNid nid
  leaves ← zipWithM (showNoteTree (width-3)) 
             (map Just [1..]) 
             (note ^. subs)
  return $ showNoteTitle width mbI (note ^. title) ++ 
           indent 3 (concat leaves)

showNoteList :: Int → [NID] → M [Text]
showNoteList width ns = 
  concat . zipWith (showNoteTitle width) (map Just [1..]) $/ 
  mapM (\n → use $ atNid n.title) ns



outputText :: Output → TBox
outputText (OutText t) = return t
outputText (OutMany s) = concatMapM outputText s
outputText (Info i)    = "INFO: " `stackLeft` outputText i
outputText (

-- | Outputs a string.
putStr :: Text → GTK ()
putStr t = do
  output ← view wOutput
  liftGTK $ do
    buffer ← G.get output textViewBuffer
    -- We create a mark at the end of text before inserting anything. It's
    -- done to be able to scroll to the beginning of output text instead of
    -- its end.
    endIter ← textBufferGetEndIter buffer
    -- 'True' here means that the mark will retain its position after text
    -- insertion.
    endMark ← textBufferCreateMark buffer Nothing endIter True
    textBufferInsert buffer endIter (unpack t)
    textViewScrollToMark
      output
      endMark        -- Actually “used-to-be-end-mark”.
      0              -- “no margin”.
      (Just (0, 0))  -- “upper left corner”.

  output2 ← view wOutput2
  liftGTK $ do
    Just doc  ← webViewGetDomDocument output2
    Just body ← documentGetBody doc
    -- Turns "hello\nworld\n" into
    -- [Just "hello", Nothing, Just "world", Nothing]
    forM_ (filter (/= Just "") $ intersperse Nothing $
           map Just $ splitOn "\n" t) $ \chunk →
      case chunk of
        Nothing → void $ nodeAppendChild body /$/
                    documentCreateElement doc "br"
        Just t  → void $ nodeAppendChild body /$/
                    documentCreateTextNode doc (unpack t)


-- | Outputs a string with a newline.
putStrLn :: Text → GTK ()
putStrLn t = putStr (t ++ "\n")


-- | Sets main window's title.
setTitle :: Text → GTK ()
setTitle s = do
  main ← view wMain
  liftGTK $ do
    G.set main [windowTitle := unpack s]


-- | @'setInput' (l, r)@ place @l ++ r@ into the input editbox, with cursor
-- placed between @l@ and @r@.
setInput :: (Text, Text) → GTK ()
setInput (left, right) = do
  command ← view wCommand
  liftGTK $ do
    buffer ← G.get command textViewBuffer
    -- Write the left part.
    G.set buffer [textBufferText := unpack left]
    -- Get cursor position.
    cursor ← textIterGetOffset /$/ textBufferGetEndIter buffer
    -- Write the right part.
    textBufferInsertAtCursor buffer (unpack right)
    -- Set the cursor to the former end.
    textBufferPlaceCursor buffer /$/ textBufferGetIterAtOffset buffer cursor


-- | @'getLine'' p@ sets prompt to @p@ and returns a line from the input
-- queue, if there is one. It doesn't block.
getLine' :: Text → GTK (Maybe Text)
getLine' p = do
  setPrompt p
  queue ← view inputQueue
  liftIO' $ atomically $ tryReadTChan queue


-- | Like 'getLine'', except that it does block.
getLine :: Text → GTK Text
getLine p = do
  setPrompt p
  queue ← view inputQueue
  liftIO' $ atomically $ readTChan queue


-- | Sets prompt and asks for lines until it gets one satisfying the
-- predicate.
-- getCondLine :: Text → (Text → Bool) → GTK Text
-- getCondLine p pred = iterateUntil pred $ getLine p


-- | Asks for lines until one satisfying the predicate comes; returns a list
-- of lines not satisfying the predicate along with one line satisfying it.
-- getLinesUntil :: Text → (Text → Bool) → GTK ([Text], Text)
-- getLinesUntil p pred = do
--   line ← getLine p
--   if pred line
--     then return ([], line)
--     else do (s, last) ← getLinesUntil p pred
--             return (line:s, last)


-- | Supposed to return width of output area in chars (used for wrapping);
-- actually always returns 78.
getTerminalWidth :: GTK Int
getTerminalWidth = return 78


-- | Brings the main window to front.
popupWindow :: GTK ()
popupWindow = do
  main ← view wMain
  liftGTK $ do
    G.set main [widgetVisible := False]
    threadDelay 10000
    G.set main [widgetVisible := True]
