import ClassyPrelude hiding (first)
import Prelude (read)
import System.IO.Unsafe
import Data.Char

import N
import Helpers


data T = T Text [T]


file :: Text
file = unsafePerformIO $ readFile "/home/yom/db.txt"


doIt t = do
  rt ← use root
  note ← newNote [] "blah"
  addSub note rt

  addInputLines ["other text"]
  execute "ed blah"

  where
    tt = map parseLine $ lines t
    parseLine :: Text → (Int, Text)
    parseLine s =
      let (sp, s2) = span isSpace s
          (n, s3) = span isDigit s2
      in  (length sp `div` 3, drop 2 s3)
