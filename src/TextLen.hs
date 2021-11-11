module TextLen where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text.Encoding
import MyPrelude

charLen :: Char -> Int
charLen = B.length . Text.Encoding.encodeUtf8 . T.singleton

charLen16 :: Char -> Int
charLen16 c = if charLen c == 4 then 2 else 1

len :: Text -> Int
len = B.length . Text.Encoding.encodeUtf8

len16 :: Text -> Int
len16 = sum . map charLen16 . T.unpack