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
  )
where

import qualified Control.Lens as L
import Control.Lens.Operators
import qualified Data.Pos as Pos
import Data.Range (Range (RangeV))
import qualified Data.Range as Range
import Data.Rope.UTF16 (Rope)
import qualified Data.Rope.UTF16 as Rope
import Data.Span (Span (SpanV, SpanV'))
import MyPrelude
import Data.Pos (Pos)

rangeInBounds :: Range -> Rope -> Bool
rangeInBounds (RangeV start end) rope =
  start <= len && end <= len
  where
    len = Rope.length rope

checkRange :: Range -> Rope -> ()
checkRange (RangeV start end) rope = ()
  where
    !_ =
      (start <= len || error "Data.Rope.Utils: start is out of bounds")
        && (end <= len || error "Data.Rope.Utils: end is out of bounds")
    len = Rope.length rope

unsafeIndexRange :: Range -> Rope -> Rope
unsafeIndexRange range@(RangeV start _end) rope =
  let (_, after) = Rope.splitAt start rope
      slice = Rope.take (Range.length range) after
   in slice

indexRange :: Range -> Rope -> Rope
indexRange range rope = checkRange range rope `seq` unsafeIndexRange range rope

getRange :: Range -> Rope -> Maybe Rope
getRange range rope =
  if rangeInBounds range rope
    then Just $ unsafeIndexRange range rope
    else Nothing

indexSpan :: Span -> Rope -> Rope
indexSpan (SpanV startPos endPos) rope = indexRange (Range.unsafeNew start end) rope
  where
    start = Rope.rowColumnCodeUnits (startPos ^. L.from Pos.rowColumn) rope
    end = Rope.rowColumnCodeUnits (endPos ^. L.from Pos.rowColumn) rope

spanInBounds :: Span -> Rope -> Bool
spanInBounds (SpanV' line1 col1 line2 col2) rope = line1 <= numLines && line2 <= numLines
  where
    numLines = Rope.rows rope

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

spanToRange :: Span -> Rope -> Range
spanToRange (SpanV startPos endPos) rope = Range.unsafeNew start end
  where
    start = Rope.rowColumnCodeUnits (startPos ^. L.from Pos.rowColumn) rope
    end = Rope.rowColumnCodeUnits (endPos ^. L.from Pos.rowColumn) rope