{-# LANGUAGE ConstraintKinds #-}


module Test where


import ClassyPrelude
import Control.Monad.Classes
import Control.Monad.Writer (runWriterT)
import Text.Printf

import N
import GUI.Fake
import Helpers
import Notes
import Command
import ClassesLens


type Test' m = (M' m, Fake' m, MonadWriter [Text] m)
type Test = Test' m => m ()


runTest :: Test → IO [Text]
runTest t = snd $/ runWriterT $ runFake $ runDb $ runAux emptyAuxState t


expect :: Bool → String → Test
expect b t = unless b $ tell [asText $ pack t]


t'edit :: Test
t'edit = do
  rt ← use root
  note ← newNote [] "blah"
  addSub note rt

  addInputLines ["other text"]
  execute "ed blah"

  t ← use $ atNid note.title
  expect (t == "other text")
    (printf "t'edit: expected \"other text\" after editing, got %s" (show t))





{- code I used to get back notes

file :: Text
file = unsafePerformIO $ readFile "/home/yom/db.txt"


doIt t = do
  enableMessages
  addInputLines [snd (headEx tt)]
  go (tailDef tt) (headEx tt) 
  execute "+/"
  
  where

    go []              _       = addInputLines [""]
    go ((i1, s1):next) (i0, _) = do
      if | i1 == i0     → addInputLines [s1]
         | i1 == i0 + 1 → addInputLines [">", s1]
         | i1 <  i0     → addInputLines (replicate (i0-i1) "" ++ [s1])
      go next (i1, s1)

    tt = map parseLine $ lines t
    parseLine :: Text → (Int, Text)
    parseLine s =
      let (sp, s2) = span isSpace s
          (n, s3) = span isDigit s2
      in  (length sp `div` 3, drop 2 s3)
-}
