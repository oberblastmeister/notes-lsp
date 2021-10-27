module LineIndex where

import Control.Foldl (Fold (Fold))
import qualified Control.Foldl as Fold
import Control.Lens hiding (Fold)
import qualified Data.ByteString as Bytestring
import qualified Data.Char as Char
import qualified Data.IntMap as IntMap
import qualified Data.List.Split as Split
import Data.Range (Range (RangeV))
import qualified Data.Range as Range
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector.Unboxed as VU
import MyPrelude

type Utf8Range = Range

data LineIndex = LineIndex
  { -- Offset of the beginning of each line, zero-based.
    -- First one is always zero
    newLines :: !(VU.Vector Int),
    -- List of non-ASCII characters on each line, zero-based
    utf16Lines :: !(IntMap (VU.Vector Utf8Range))
  }
  deriving (Show, Eq)

utf8RangeUtf16 :: Range -> Int
utf8RangeUtf16 rc =
  if Range.length rc == 4
    then 2
    else 1

charLen :: Char -> Int
charLen = Bytestring.length . Text.Encoding.encodeUtf8 . T.singleton


endByKeepR :: (a -> Bool) -> [a] -> [[a]]
endByKeepR = Split.split . Split.dropFinalBlank . Split.keepDelimsR . Split.whenElt

lines' :: [(Char, Int)] -> [[(Char, Int)]]
lines' = endByKeepR (\tup -> tup ^. _1 == '\n')

addRange :: [(Char, Int)] -> [(Char, Int, Utf8Range)]
addRange =
  drop 1
    . scanl'
      ( \(_, _, RangeV _ end) (c, len) ->
          ( c,
            len,
            Range.new end (end + len)
          )
      )
      (error "", error "", Range.new 0 0)

utf16LineFold :: Fold (Char, Int, Utf8Range) (VU.Vector Utf8Range)
utf16LineFold =
  Fold
    ( \v (c, _, rc) ->
        if Char.isAscii c
          then v
          else VU.snoc v rc
    )
    VU.empty
    id

lineFold :: [(Char, Int, Utf8Range)] -> (VU.Vector Utf8Range, Int, Bool)
lineFold =
  Fold.fold
    ( (,,) <$> utf16LineFold
        <*> (Fold.last <&> (^?! each . _3 . Range._end)) -- invariant: the line cannot be empty
        <*> (Fold.last <&> (^?! each . _1 . to (== '\n')))
    )

newLinesFold :: Fold ((VU.Vector Utf8Range, Int, Bool), Int) (VU.Vector Int)
newLinesFold =
  Fold
    ( \v ((_, lastEnd, hasNewline), _) ->
        if hasNewline
          then VU.snoc v lastEnd
          else v
    )
    (VU.singleton 0) -- the first line offset is 0
    id

utf16LinesFold :: Fold ((VU.Vector Utf8Range, Int, Bool), Int) (IntMap (VU.Vector Utf8Range))
utf16LinesFold =
  Fold
    ( \im ((ranges, _, _), lNum) ->
        if VU.null ranges
          then im
          else im & at lNum ?~ ranges
    )
    IntMap.empty
    id

lineIndexFromData :: [((VU.Vector Utf8Range, Int, Bool), Int)] -> LineIndex
lineIndexFromData = Fold.fold (LineIndex <$> newLinesFold <*> utf16LinesFold)

makeLineIndex :: Text -> LineIndex
makeLineIndex =
  T.unpack
    >>> map (\c -> (c, charLen c))
    >>> lines'
    >>> map addRange
    >>> map lineFold
    >>> (`zip` [0 ..])
    >>> lineIndexFromData