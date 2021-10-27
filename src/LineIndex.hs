module LineIndex where

import Control.Foldl (Fold (Fold))
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import Control.Lens.Operators
import qualified Data.ByteString as Bytestring
import qualified Data.Char as Char
import qualified Data.IntMap as IntMap
import qualified Data.List.Split as Split
import Data.Range (Range (RangeV))
import qualified Data.Range as Range
import qualified Data.Text as T
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as VU
import MyPrelude
import Utils (scanl'')

type Offset :: IndexKind -> *
newtype Offset a = Offset {unOffset :: Int}
  deriving
    ( Show,
      Eq,
      Ord,
      Num
    )

type Utf8Offset = Offset 'Utf8

type CharOffset = Offset 'Char

type Utf16Offset = Offset 'Utf16

utf8Offset' :: Int -> Utf8Offset
utf8Offset' = coerce

utf16Offset' :: Int -> Utf16Offset
utf16Offset' = coerce

charOffset' :: Int -> CharOffset
charOffset' = coerce

type Utf8Range = Range

data LineIndex = LineIndex
  { -- Offset of the beginning of each line, zero-based.
    -- First one is always zero
    newLines :: !(VU.Vector Int),
    -- List of non-ASCII characters on each line, zero-based
    nonAscii :: !(IntMap (VU.Vector Utf8Range))
  }
  deriving (Show, Eq, Generic)

data IndexKind = Utf8 | Utf16 | Char

-- type OtherEncoding :: IndexKind -> IndexKind

type family OtherEncoding a where
  OtherEncoding 'Utf8 = 'Utf16
  OtherEncoding 'Utf16 = 'Utf8

type LineCol :: IndexKind -> *
data LineCol a = LineCol
  { line :: !Int,
    col :: !Int
  }
  deriving (Show, Eq, Generic)

type Utf8LineCol = LineCol 'Utf8

type Utf16LineCol = LineCol 'Utf16

data SEncodingKind a where
  SUtf8 :: SEncodingKind 'Utf8
  SUtf16 :: SEncodingKind 'Utf16
  SChar :: SEncodingKind 'Char

type SingEncodingKind :: IndexKind -> Constraint
class SingEncodingKind a where
  sing :: SEncodingKind a

instance SingEncodingKind 'Utf8 where
  sing = SUtf8

instance SingEncodingKind 'Utf16 where
  sing = SUtf16

instance SingEncodingKind 'Char where
  sing = SChar

-- | Returns the index of the partition point according to the given predicate
-- | (the index of the first element of the second partition).
partitionPoint :: V.Vector v a => (a -> Bool) -> v a -> Int
partitionPoint f v = go 0 (V.length v)
  where
    go :: Int -> Int -> Int
    go left right
      | left == right = left
      | otherwise =
        let mid = left + (right - left) `div` 2
            val = v V.! mid
            (left', right') =
              if f val
                then (mid + 1, right)
                else (left, mid)
         in go left' right'

utf16Len :: Utf8Range -> Int
utf16Len r =
  if Range.length r == 4
    then 2
    else 1

utf8Len :: Utf8Range -> Int
utf8Len = Range.length

lenDiff :: Utf8Range -> Int
lenDiff r = utf8Len r - utf16Len r

lenToUtf16 :: Int -> Int
lenToUtf16 i = if i == 4 then 2 else 1

charLen :: Char -> Utf8Offset
charLen = utf8Offset' . Bytestring.length . Text.Encoding.encodeUtf8 . T.singleton

charLenUtf16 :: Char -> Utf16Offset
charLenUtf16 = utf16Offset' . lenToUtf16 . unOffset . charLen

endByKeepR :: (a -> Bool) -> [a] -> [[a]]
endByKeepR = Split.split . Split.dropFinalBlank . Split.keepDelimsR . Split.whenElt

new :: Text -> LineIndex
new =
  T.unpack
    >>> map (\c -> (c, unOffset $ charLen c))
    >>> scanl'' (\(_, _, !offset) (c, len) -> (c, len, offset + len)) (error "", error "", 0)
    >>> lines'
    >>> map addRange
    >>> map lineFold
    >>> (`zip` [0 ..])
    >>> lineIndexFromData
  where
    addRange =
      scanl''
        ( \(_, _, RangeV _ end) (c, len, offset) ->
            ( c,
              offset,
              Range.new end (end + len)
            )
        )
        (error "", error "", Range.new 0 0)

    lines' = endByKeepR (^. L._1 . L.to (== '\n'))

    utf16LineFold =
      Fold
        ( \v (c, _, rc) ->
            if Char.isAscii c
              then v
              else VU.snoc v rc
        )
        VU.empty
        id

    lineFold =
      Fold.fold
        ( (,,) <$> utf16LineFold
            <*> (Fold.last <&> (^?! L.each . L._2)) -- invariant: the line cannot be empty
            <*> (Fold.last <&> (^?! L.each . L._1 . L.to (== '\n')))
        )

    newLinesFold =
      Fold
        ( \v ((_, lastEnd, hasNewline), _) ->
            if hasNewline
              then VU.snoc v lastEnd
              else v
        )
        (VU.singleton 0) -- the first line offset is 0
        id

    utf16LinesFold =
      Fold
        ( \im ((ranges, _, _), lNum) ->
            if VU.null ranges
              then im
              else im & L.at lNum ?~ ranges
        )
        IntMap.empty
        id

    lineIndexFromData = Fold.fold (LineIndex <$> newLinesFold <*> utf16LinesFold)

-- | offset is in utf8
lineCol :: Utf8Offset -> LineIndex -> Utf8LineCol
lineCol offset lineIndex = LineCol {line, col = col}
  where
    offset' = coerce offset :: Int
    newLines = lineIndex ^. #newLines
    line = partitionPoint (<= offset') newLines - 1
    lineStartOffset = newLines ^?! L.ix line
    col = offset' - lineStartOffset

lineColOffset :: Utf8LineCol -> LineIndex -> Utf8Offset
lineColOffset LineCol {line, col} lineIndex = lineIndex ^?! #newLines . L.ix line . L.to coerce + coerce col

lineColToUtf8 :: Utf8LineCol -> LineIndex -> Utf16LineCol
lineColToUtf8 LineCol {line, col} lineIndex = LineCol {line, col = utf8ToUtf16Col line col lineIndex}

lineColToUtf16 :: Utf16LineCol -> LineIndex -> Utf8LineCol
lineColToUtf16 LineCol {line, col} lineIndex = LineCol {line, col = utf16ToUtf8Col line col lineIndex}

utf16ToUtf8Col :: Int -> Int -> LineIndex -> Int
utf16ToUtf8Col line col lineIndex =
  ( lineIndex ^. #nonAscii . L.at line & L._Just
      %~ ( (^.. L.each)
             >>> map (\r -> (r, lenDiff r))
             >>> scanl' (\t (r, diff) -> (r, diff, t ^. L._3' + diff)) (error "", error "", 0)
             >>> drop 1
             >>> takeWhile (\t -> col <= t ^. L._1 . Range._end)
             >>> map (^. L._2)
             >>> foldl' (+) col
         )
  )
    ^. L.non col

utf8ToUtf16Col :: Int -> Int -> LineIndex -> Int
utf8ToUtf16Col line col lineIndex =
  ( lineIndex ^. #nonAscii . L.at line & L._Just
      %~ ( (^.. L.each)
             >>> takeWhile (^. Range._end . L.to (<= col))
             >>> map lenDiff
             >>> foldl' (-) col
         )
  )
    ^. L.non col

charToUtf16Col :: Int -> Int -> LineIndex -> Int
charToUtf16Col line col lineIndex =
  ( lineIndex ^. #nonAscii . L.at line & L._Just
      %~ ( (^.. L.each)
             >>> take col
             >>> map utf16Len
             >>> sum
         )
  )
    ^. L.non col