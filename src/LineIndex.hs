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

-- data Test = Test {line :: Int}

data LineIndex = LineIndex
  { -- Offset of the beginning of each line, zero-based.
    -- First one is always zero
    newLines :: !(VU.Vector Int),
    -- List of non-ASCII characters on each line, zero-based
    nonAscii :: !(IntMap (VU.Vector Utf8Range))
  }
  deriving (Show, Eq, Generic)

data LineColKind = Utf8 | Utf16 | Char

-- type OtherEncoding :: LineColKind -> LineColKind

-- type family OtherEncoding a where
--   OtherEncoding 'Utf8 = 'Utf16
--   OtherEncoding 'Utf16 = 'Utf8

type LineCol :: LineColKind -> *
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

type SingEncodingKind :: LineColKind -> Constraint
class SingEncodingKind a where
  sing :: SEncodingKind a

instance SingEncodingKind 'Utf8 where
  sing = SUtf8

instance SingEncodingKind 'Utf16 where
  sing = SUtf16

instance SingEncodingKind 'Char where
  sing = SChar

utf16Len :: Utf8Range -> Int
utf16Len r =
  if Range.length r == 4
    then 2
    else 1

utf8Len :: Utf8Range -> Int
utf8Len = Range.length

lenDiff :: Utf8Range -> Int
lenDiff r = utf8Len r - utf16Len r

charLen :: Char -> Int
charLen = Bytestring.length . Text.Encoding.encodeUtf8 . T.singleton

endByKeepR :: (a -> Bool) -> [a] -> [[a]]
endByKeepR = Split.split . Split.dropFinalBlank . Split.keepDelimsR . Split.whenElt

lines' :: [(Char, Int)] -> [[(Char, Int)]]
lines' = endByKeepR (^. _1 . to (== '\n'))

new :: Text -> LineIndex
new =
  T.unpack
    >>> map (\c -> (c, charLen c))
    >>> lines'
    >>> map addRange
    >>> map lineFold
    >>> (`zip` [0 ..])
    >>> lineIndexFromData

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

-- changeCol :: forall a. SingEncodingKind a => LineIndex -> LineCol a -> LineCol (OtherEncoding a)
-- changeCol lineIndex LineCol {line, col} = case sing @a of
--   SUtf8 -> LineCol {line = utf8toUtf16Col lineIndex line col, col}
--   SUtf16 -> LineCol {line = utf16toUtf8Col lineIndex line col, col}

-- lineColToOffset :: LineCol
-- lineCol :: Int -> LineIndex -> Utf8LineCol
-- lineCol offset lineIndex =

utf16ToUtf8Col :: Int -> Int -> LineIndex -> Int
utf16ToUtf8Col line col lineIndex =
  ( lineIndex ^. #nonAscii . at line & _Just
      %~ ( (^.. each)
             >>> takeWhile (^. Range._end . to (<= col))
             >>> map lenDiff
             >>> foldl' (+) col
         )
  )
    ^. non col

utf8ToUtf16Col :: Int -> Int -> LineIndex -> Int
utf8ToUtf16Col line col lineIndex =
  ( lineIndex ^. #nonAscii . at line & _Just
      %~ ( (^.. each)
             >>> takeWhile (^. Range._end . to (<= col))
             >>> map lenDiff
             >>> foldl' (-) col
         )
  )
    ^. non col

charToUtf16Col :: Int -> Int -> LineIndex -> Int
charToUtf16Col line col lineIndex =
  ( lineIndex ^. #nonAscii . at line & _Just
      %~ ( (^.. each)
             >>> take col
             >>> map utf16Len
             >>> sum
         )
  )
    ^. non col