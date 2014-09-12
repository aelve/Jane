{-# LANGUAGE
  ConstraintKinds #-}


module Helpers (
  (==^), (=^=), (^==),
  isSubseq, isSubseqBy, separate,
  times, lenWhile, concatMapM,
  check, tryCheck, consumeRest, spaces1, text,
  insertFromTo, swapList,
  module Data.Bifunctor,
  ifM, ($/), (/$/),
  liftIO', IO_, IO',
  liftSTM', STM_, STM',
  registerMainThread, mainExitSuccess )
  where


import ClassyPrelude hiding (try)
import Text.Parsec hiding (uncons)
import Text.Parsec.Text
import Data.Bifunctor
import Control.Monad.Base
import Text.Printf
import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (throwTo)
import System.Exit (ExitCode(..))
import System.IO.Unsafe (unsafePerformIO)


{- ======================================================================== -}
-- |
-- = List/sequence functions


-- | @a ==^ b@ checks whether @a@ is suffix of @b@.
(==^) :: EqSequence seq => seq → seq → Bool
(==^) = isSuffixOf


-- | @a =^= b@ checks whether @a@ is contained in @b@.
(=^=) :: EqSequence seq => seq → seq → Bool
(=^=) = isInfixOf


-- | @a ^== b@ checks whether @a@ is prefix of @b@.
(^==) :: EqSequence seq => seq → seq → Bool
(^==) = isPrefixOf


-- | @isSubseq a b@ checks whether @a@ is a subsequence of @b@.
isSubseq :: Eq a => [a] → [a] → Bool
isSubseq = isSubseqBy (==)


-- | @isSubseq eq a b@ checks whether @a@ is a subsequence of @b@ if elements
-- are compared by @eq@.
isSubseqBy :: (a → b → Bool) → [a] → [b] → Bool
isSubseqBy _ [] _ = True
isSubseqBy eq (a:as) (b:bs) 
  | a `eq` b  = isSubseqBy eq as bs
  | otherwise = isSubseqBy eq (a:as) bs
isSubseqBy _ _ [] = False


-- | @lenWhile p seq@ finds the length of maximum prefix of @seq@ consisting
-- of elements satisfying the predicate.
-- 
-- >>> lenWhile isUpper "ABCaBaCaBa"
-- 3
-- 
lenWhile :: IsSequence seq => (Element seq → Bool) → seq → Int
lenWhile p = length . takeWhile p


-- | Repeats a sequence.
-- 
-- >>> times 10 "abc"
-- "abcabcabcabcabcabcabcabcabcabc"
-- 
times :: (IsSequence seq) => Int → seq → seq
times i = concat . asList . replicate i


-- | Swaps two elements in the list. If any of indexes is outside the
-- boundaries, an exception is raised.
-- 
-- >>> swapList 2 7 [0..9]
-- [0,1,7,3,4,5,6,2,8,9]
-- 
-- These properties hold:
-- 
-- prop> swapList i j s !! j == s !! i
-- prop> swapList i j s !! i == s !! j
-- prop> (k /= i && k /= j) => s !! k == swapList i j s !! k 
-- 
swapList :: Int → Int → [a] → [a]
swapList i j s 
  | i < 0     = error "swapList: from < 0"
  | i >= len  = error "swapList: from >= len"
  | j < 0     = error "swapList: to < 0"
  | j >= len  = error "swapList: to >= len"
  | i == j    = s
  | i >  j    = swapList j i s
  | otherwise = let (l,  x:r)  = splitAt i s
                    (lr, y:rr) = splitAt (j-i-1) r
                in  l ++ [y] ++ lr ++ [x] ++ rr
  where len = length s


-- | Extracts an element from specified position in the list and inserts it
-- in another position. If any of indexes is outside the boundaries, an
-- exception is raised.
-- 
-- >>> insertFromTo 2 7 [0..9]
-- [0,1,3,4,5,6,7,2,8,9]
-- 
-- This property holds:
-- 
-- prop> insertFromTo i j s !! j == s !! i
-- 
insertFromTo :: Int → Int → [a] → [a]
insertFromTo i j s 
  | i < 0     = error "insertFromTo: from < 0"
  | i >= len  = error "insertFromTo: from >= len"
  | j < 0     = error "insertFromTo: to < 0"
  | j >= len  = error "insertFromTo: to >= len"
  | otherwise = let (l, x:r) = splitAt i s
                    (l', r') = splitAt j (l ++ r) 
                in  l' ++ [x] ++ r'
  where len = length s


concatMapM f = fmap concat . mapM f


-- | Separates elements of the list into chunks – 'Left's don't satisfy the
-- predicate, 'Right's do.
-- 
-- >>> separate isUpper "lowerUPPERloweragain"
-- [Left "lower",Right "UPPER",Left "loweragain"]
-- 
separate :: IsSequence seq => (Element seq → Bool) → seq → [Either seq seq]
separate p s = go s
  where
    go s
      | Just x ← headMay s = if p x
          then let (l, r) = span  p s
               in Right l : go r
          else let (l, r) = break p s
               in Left  l : go r
      | otherwise          = []


{- ======================================================================== -}
-- |
-- = Parsing


-- | Returns 'True' if the parse will be successful. Does not consume input.
tryCheck :: Parser a → Parser Bool
tryCheck p = isJust $/ optionMaybe (lookAhead (try p))


-- | Parses and returns 'True' if the parse was successful; if it wasn't,
-- does not consume input.
check :: Parser a → Parser Bool
check p = isJust $/ optionMaybe (try p)


-- | Consumes the rest of input.
consumeRest :: Parser Text
consumeRest = pack $/ anyChar `manyTill` eof


-- | Parses one or more spaces.
spaces1 = space *> spaces


-- | Same as 'string', but for anything 'Textual'.
text :: Textual t => t → Parser t
text t = pack $/ string (unpack t)


{- ======================================================================== -}
-- |
-- = Monad constraints and specialized functions.


-- |
-- == IO


-- | 'liftIO' for 'MonadBase'.
liftIO' :: MonadBase IO m => IO a → m a
liftIO' = liftBase


-- | A constraint for monads which support IO (as 'MonadBase').
type IO' m = MonadBase IO m


-- | A placeholder for monads which support IO (as 'MonadBase').
-- 
-- @
--     type IO_ a = MonadBase IO m => m a
-- @
type IO_ a = IO' m => m a


-- |
-- == STM


-- | 'liftSTM' for 'MonadBase'.
liftSTM' :: MonadBase STM m => STM a → m a
liftSTM' = liftBase


-- | A constraint for monads which support STM (as 'MonadBase').
type STM' m = MonadBase STM m


-- | A placeholder for monads which support STM (as 'MonadBase').
-- 
-- @
--     type STM_ a = MonadBase STM m => m a
-- @
type STM_ a = STM' m => m a


{- ======================================================================== -}
-- |
-- = Various


-- | Monadic version of the 'if' condition.
ifM :: Monad m => m Bool → m a → m a → m a
ifM b t f = do
  ba <- b
  if ba then t else f


-- | '<$>' with the same fixity as '$'. 
($/) :: Functor f => (a → b) → f a → f b
($/) = (<$>)

infixr 0 $/


-- | '=<<' with the same fixity as '$'.
(/$/) :: Monad m => (a → m b) → m a → m b
(/$/) = (=<<)

infixr 0 /$/


-- | An instance to make 'printf' support 'Text'.
instance PrintfArg Text where
    formatArg = formatString . unpack


mainThreadIDVar :: IORef (Maybe ThreadId)
mainThreadIDVar = unsafePerformIO $ newIORef Nothing


-- | Registers the callee of this action as the main thread.
registerMainThread :: IO ()
registerMainThread = writeIORef mainThreadIDVar . Just /$/ myThreadId


-- | 'exitSuccess' which can be called from any thread but would raise an
-- exception in the main thread (which has to be registered with
-- 'registerMainThread'). If the thread wasn't registered, doesn't do
-- anything; otherwise, unregisters it (since it's presumably going to be
-- killed).
mainExitSuccess :: IO ()
mainExitSuccess = do
  mbMain ← readIORef mainThreadIDVar
  case mbMain of
    Nothing   → return ()
    Just main → do
      writeIORef mainThreadIDVar Nothing
      throwTo main ExitSuccess
