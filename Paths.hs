{-# LANGUAGE
  RankNTypes, ScopedTypeVariables #-}

module Paths where

import ClassyPrelude

import Patterns

data Sel = SelTitle TitlePat | SelIndex Int

instance Show Sel where
  show (SelTitle t) = unpack $ renderTitlePat t
  show (SelIndex i) = show i 

data MSel = MSelList [Sel]

instance Show MSel where
  show (MSelList ss) = intercalate "," (map show ss)

-- | Just a path to a note.
type Path = [Sel]

showPath :: Path -> Text
showPath = intercalate "/" . map tshow

-- | A path which chooses multiple notes.
type MPath = [MSel]

showMPath :: MPath -> Text
showMPath = intercalate "/" . map tshow

-- | Currently is the same as root path.
currentNote :: Path
currentNote = []

rootNote :: Path
rootNote = []
