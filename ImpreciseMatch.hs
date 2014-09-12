module ImpreciseMatch (
  )
  where

import ClassyPrelude
import Data.Text.ICU

data DiaMark =
  Grave | Acute | Circumflex | Tilde | Macron | Overline | Breve |
  DotAbove | Diaeresis | HookAbove | RingAbove | DoubleAcute |
  Caron -- INCOMPLETE!
  deriving (Eq, Ord, Show)

data CharProp =
  Uppercase | Lowercase | Superscript | Subscript | Diacritics (Set DiaMark)
  deriving (Eq, Ord, Show)

data Unit = Unit {
  baseChar  :: Char,
  charProps :: Set CharProp }
  deriving (Eq, Show)

decomposeChar :: Char -> Unit
decomposeChar c = undefined -- fromMaybe (Decomposed c mempty) (lookup c all)
  where
    mkPs s b _ | length s /= length b = error
      "ImpreciseMatch.decomposeChar.mkPs: lengths mismatch (internal error)"
    mkPs s b d = zip s $ Unit <$> b <*> pure (setFromList d)

    all = undefined

decomposeText :: Text -> [Unit]
decomposeText t = breaks (breakCharacter Current)

{-

thing   : matched by
--------------------

ёлка    : елка
елка    : ёлка

åwesome : awesome

café    : cafe
cafe    × café  //nope?

йопт    × иопт

-}
