module ImpreciseMatch (
  )
  where

import ClassyPrelude

data DiaMark =
  Grave | Acute | Circumflex | Tilde | Macron | Overline | Breve |
  DotAbove | Diaeresis | HookAbove | RingAbove | DoubleAcute |
  Caron | VerticalLineAbove | DoubleVerticalLineAbove | DoubleGrave |
  Candrabindu | InvertedBreve | TurnedCommaAbove | CommaAbove |
  ReversedCommaAbove -- INCOMPLETE!

data CharProp =
  Uppercase | Lowercase | Superscript | Subscript | Diacritics [DiaMark]

decomposeChar :: 
