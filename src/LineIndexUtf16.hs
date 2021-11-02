module LineIndexUtf16 where

import qualified Data.ByteString as Bytestring
import Data.Range (Range)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector.Unboxed as VU
import MyPrelude

charLenUtf8 :: Char -> Int
charLenUtf8 = Bytestring.length . Text.Encoding.encodeUtf8 . T.singleton

lenToUtf16 :: Int -> Int
lenToUtf16 i = if i == 4 then 2 else 1

charLenUtf16 :: Char -> Int
charLenUtf16 = lenToUtf16 . charLenUtf8

data LineIndex = LineIndex
  { -- List of non-ASCII characters on each line, zero-based
    -- Each non-ASCII character is a utf-16 range
    nonAscii :: !(IntMap (VU.Vector Range))
  }
  deriving (Show, Eq, Generic)

-- new :: Text -> LineIndex
-- new =
--   T.unpack