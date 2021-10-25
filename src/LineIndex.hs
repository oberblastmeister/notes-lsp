module LineCol where

import qualified Data.ByteString as Bytestring
import Data.Range (Range)
import qualified Data.Range as Range
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector.Unboxed as VU
import MyPrelude

newtype RangeChar = RangeChar {unUtf16Char :: Range} deriving (Show, Eq, Ord)

rangeCharLen :: RangeChar -> Int
rangeCharLen = Range.length . unUtf16Char

rangeCharLenUtf16 :: RangeChar -> Int
rangeCharLenUtf16 c =
  if rangeCharLen c == 4
    then 2
    else 1

charLen :: Char -> Int
charLen = Bytestring.length . Text.Encoding.encodeUtf8 . T.singleton

data LineIndex = LineIndex
  { newLines :: !(VU.Vector Range),
    utf16Lines :: !(IntMap (VU.Vector RangeChar))
  }

lineIndexFromText :: Text -> LineIndex
lineIndexFromText = do
  undefined
  where
    go ::
      Text ->
      LineIndex ->
      VU.Vector RangeChar ->
      Int ->
      Int ->
      LineIndex
    go t lineIndex utf16Chars row col
      | T.null t = lineIndex
      | otherwise = undefined