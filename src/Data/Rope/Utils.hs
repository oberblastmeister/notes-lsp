module Data.Rope.Utils
  ( checkRange,
    unsafeIndexRange,
    indexRange,
    indexSpan,
    text,
    getSpan,
    getRange,
    spanToRange,
    posToOffset,
    offsetToPos,
    rangeToSpan,
  )
where

import qualified Control.Lens as L
import Control.Lens.Operators
import Data.Pos (Pos)
import qualified Data.Pos as Pos
import Data.Range (Range (RangeV))
import qualified Data.Range as Range
import Data.Rope.UTF16 (Rope)
import qualified Data.Rope.UTF16 as Rope
import qualified Data.Rope.UTF16 as Ropw
import Data.Span (Span (SpanV, SpanV'))
import qualified Data.Span as Span
import MyPrelude

rangeInBounds :: Range -> Rope -> Bool
rangeInBounds (RangeV start end) rope =
  start <= len && end <= len
  where
    len = fromIntegral $ Rope.length rope

checkRange :: Range -> Rope -> ()
checkRange (RangeV start end) rope = ()
  where
    !_ =
      (start <= len || error "Data.Rope.Utils: start is out of bounds")
        && (end <= len || error "Data.Rope.Utils: end is out of bounds")
    len = fromIntegral $ Rope.length rope

unsafeIndexRange :: Range -> Rope -> Rope
unsafeIndexRange range@(RangeV start _end) rope =
  let (_, after) = Rope.splitAt (fromIntegral start) rope
      slice = Rope.take (fromIntegral $ Range.length range) after
   in slice

indexRange :: Range -> Rope -> Rope
indexRange range rope = checkRange range rope `seq` unsafeIndexRange range rope

getRange :: Range -> Rope -> Maybe Rope
getRange range rope =
  if rangeInBounds range rope
    then Just $ unsafeIndexRange range rope
    else Nothing

indexSpan :: Span -> Rope -> Rope
indexSpan span rope = indexRange (Range.unsafeNew start end) rope
  where
    (RangeV start end) = spanToRange span rope

spanInBounds :: Span -> Rope -> Bool
spanInBounds (SpanV' line1 col1 line2 col2) rope = line1 <= numLines && line2 <= numLines
  where
    numLines = fromIntegral $ Rope.rows rope

-- warning: this will only check if the lines are in bounds
-- it cannot check if the columns are in bounds
getSpan :: Span -> Rope -> Maybe Rope
getSpan span rope =
  if spanInBounds span rope
    then getRange (spanToRange span rope) rope
    else Nothing

text :: L.Iso' Rope Text
text = L.iso Rope.toText Rope.fromText

posToOffset :: Pos -> Rope -> Int
posToOffset pos = Rope.rowColumnCodeUnits (pos ^. L.from Pos.rowColumn)

offsetToPos :: Int -> Rope -> Pos
offsetToPos offset rope = Ropw.codeUnitsRowColumn offset rope ^. Pos.rowColumn

spanToRange :: Span -> Rope -> Range
spanToRange (SpanV startPos endPos) rope = Range.unsafeNew start end
  where
    start = posToOffset startPos rope
    end = posToOffset endPos rope

rangeToSpan :: Range -> Rope -> Span
rangeToSpan (RangeV start end) rope = Span.unsafeNew startPos endPos
  where
    startPos = offsetToPos start rope
    endPos = offsetToPos end rope
