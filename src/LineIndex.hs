module LineIndex where

import Control.Foldl (Fold (Fold))
import Control.Lens hiding (Fold)
import qualified Data.ByteString as Bytestring
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Range (Range (RangeV))
import qualified Data.Range as Range
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import MyPrelude

-- zero based
-- inclusive of start
-- exclusive of end
rangeCharLenUtf16 :: Range -> Int
rangeCharLenUtf16 rc =
  if Range.length rc == 4
    then 2
    else 1

charLen :: Char -> Int
charLen = Bytestring.length . Text.Encoding.encodeUtf8 . T.singleton

type RangeChar = Range

data LineIndex = LineIndex
  { newLines :: !(VU.Vector Int),
    utf16Lines :: !(IntMap (VU.Vector RangeChar))
  }

addCharLen :: [Char] -> [(Char, Int)]
addCharLen = map (\c -> (c, charLen c))

addCol :: [(Char, Int)] -> [(Char, Int, Range)]
addCol =
  drop 1
    . scanl'
      ( \(_, _, RangeV _ end) (c, len) ->
          ( c,
            len,
            Range.new end (end + len)
          )
      )
      (error "", error "", Range.new 0 0)

utf16LineFold :: Fold (Char, Int, RangeChar) (VU.Vector RangeChar)
utf16LineFold =
  Fold
    ( \v (c, _, rc) ->
        if Char.isAscii c
          then v
          else VU.snoc v rc
    )
    VU.empty
    id

-- sum :: Num a => Fold a a
-- sum = Fold (+) 0 id

-- utf16LineFold :: Fold (Char, Int, Int)

-- infoWithLine :: [[Char]] -> Int -> [[Char, (Int, Int, Int)]]
-- infoWithLine lns start = List.scanl'

-- defaultLineIndex :: LineIndex
-- defaultLineIndex = LineIndex { newLines = VU.Vector.new}

-- fromText :: Text -> LineIndex
-- fromText text = do
-- lineIndexFromText :: Text -> LineIndex
-- lineIndexFromText text = do
--   undefined
--   where
--     -- go text

--     go ::
--       Text ->
--       LineIndex ->
--       (DList Range, DList (Int, RangeChar))
--         VU.Vector
--         RangeChar ->
--       Int ->
--       Int ->
--       LineIndex
--     go t lineIndex utf16Chars row col
--       | T.null t = lineIndex
--       | otherwise = undefined

-- groupByLines :: [Char] -> [[Char]]
-- groupByLines = Split.endBy "\n"