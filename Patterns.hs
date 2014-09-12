module Patterns (
  TitlePat, matchTitle, titlePatP, renderTitlePat )
  where

import ClassyPrelude hiding (try)
import Data.Char (isAlphaNum)
import Data.Text (split)
import Text.Parsec
import Text.Parsec.Text

import Helpers


data TitlePat = TitlePat [WordPat]

data WordPat = WordPat {
  wPat      :: Text,
  wIsPrefix :: Bool }  -- True if the word ends with ‘.’

matchWord :: WordPat -> Text -> Bool
matchWord w t 
  | wIsPrefix w = toCaseFold (wPat w) ^== toCaseFold t
  | otherwise   = toCaseFold (wPat w)  == toCaseFold t

matchTitle :: TitlePat -> Text -> Bool
matchTitle (TitlePat p) =
  isSubseqBy matchWord p . 
  filter (not . null) . 
  split (not . isAlphaNum)

titlePatP :: Parser TitlePat
titlePatP = TitlePat <$> wordPatP `sepBy1` string "-"

wordPatP :: Parser WordPat
wordPatP = WordPat 
  <$> (cons <$> letter <*> (pack <$> many alphaNum))
  <*> check (string ".")

renderTitlePat :: TitlePat -> Text
renderTitlePat (TitlePat ts) = intercalate "-" $ map renderWordPat ts

renderWordPat :: WordPat -> Text
renderWordPat w = wPat w ++ if wIsPrefix w then "." else ""
