{-# LANGUAGE
  RankNTypes, NoImplicitPrelude, OverloadedStrings,
  MultiWayIf, ScopedTypeVariables #-}

module Parse where

import ClassyPrelude
import Control.Applicative
import Text.Parsec hiding ((<|>), many)
import Text.Parsec.Text
import Data.Text (replace)
import Data.Char (isSpace, isSymbol, isPunctuation)
import Data.Maybe (fromJust)

import Helpers
import Patterns
import Paths

data Token =
  TPath MPath |
  TMalformedPath Text Text |
  TCommand Text |
  TString Text |
  TUnfinishedString Text
  deriving (Show)

natural :: (Read a, Integral a) => Parser a
natural = fmap (fromJust . readMay) $ 
  (:) <$> oneOf "123456789" 
      <*> many (oneOf "0123456789") 

pathP :: Parser Path
pathP = do
  absolute <- check (char '/')
  if absolute
    then selP `sepBy`  char '/'
    -- We don't want to accept empty paths.
    else selP `sepBy1` char '/'

mpathP :: Parser MPath
mpathP = do
  absolute <- check (char '/')
  if absolute
    then mselP `sepBy`  char '/'
    -- We don't want to accept empty paths.
    else mselP `sepBy1` char '/'

-- | Parses titles and indices.
selP :: Parser Sel
selP = choice [
  SelTitle <$> titlePatP,
  SelIndex <$> natural ]

mselP :: Parser MSel
mselP = MSelList <$> selP `sepBy1` char ','

isOpStarter c = (isSymbol c || isPunctuation c) && (c /= '/') 

isOpBreaker c = isSpace c || c == '`' || c == '/'

lex :: Text -> [((Int, Int), Token)]
lex t = go 0 t
  where
    gostr i t
      | null t     = (i-1, False)
      | "``" ^== t = gostr (i+2) (drop 2 t)
      | "`"  ^== t = (i, True)
      | otherwise  = gostr (i+1) (drop 1 t)
    go i t
      | null t             = []
      | isSpace h          = go (i + spacesLen) (drop spacesLen t)
      | h == '`' && strFin = ((i, strI), TString str) :
                               go (strI+1) (drop strLen t)
      | h == '`'           = [((i, strI), TUnfinishedString str)]
      | isOpStarter h      = ((i, opI), TCommand op) :
                               go (opI+1) (drop opLen t)
      | Left err <- ePath  = ((i, pathI), TMalformedPath (tshow err) path) :
                               go (pathI+1) (drop pathLen t)
      | Right pt <- ePath  = ((i, pathI), TPath pt) :
                               go (pathI+1) (drop pathLen t)
      where
        h = headEx t

        spacesLen      = lenWhile isSpace t

        (strI, strFin) = gostr (i+1) (drop 1 t)
        strLen         = strI - i + 1
        str            = replace "``" "`" $ take (strLen-2) (drop 1 t)

        opLen          = 1 + lenWhile (not . isOpBreaker) (drop 1 t)
        opI            = i + opLen - 1
        op             = take opLen t

        pathLen        = 1 + lenWhile (not . isSpace) (drop 1 t)
        pathI          = i + pathLen - 1
        path           = take pathLen t
        ePath          = parse (mpathP <* eof) "" path
