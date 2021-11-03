-- for suppressing warnings for lens constraints
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Span
  ( Span (SpanV, SpanV'),
    new,
    new',
    _start,
    _end,
    _line1,
    _line2,
    _col1,
    _col2,
    unsafeFromSourceRange,
    contains,
    empty,
    span,
  )
where

import Commonmark (SourcePos)
import qualified Control.Lens as L
import Control.Lens.Operators
import Data.Pos (Pos (..))
import qualified Data.Pos as Pos
import qualified Language.LSP.Types as LSP
import MyPrelude hiding (empty)

data Span = Span
  { start :: !Pos,
    end :: !Pos
  }
  deriving (Show, Eq, Ord)

new :: Pos -> Pos -> Span
new start@Pos {line, col} end@Pos {line = line', col = col'} =
  let !_ = if line <= line' then () else error "Data.Span.new: start line must be less end line"
      !_ = if col <= col' then () else error "Data.Span.new: start col must be less than end col"
   in Span {start, end}

new' :: Int -> Int -> Int -> Int -> Span
new' line col line' col' = new (Pos line col) (Pos line' col')

empty :: Pos -> Span
empty pos = Span {start = pos, end = pos}

pattern SpanV :: Pos -> Pos -> Span
pattern SpanV start end <- Span {start, end}

{-# COMPLETE SpanV #-}

pattern SpanV' :: Int -> Int -> Int -> Int -> Span
pattern SpanV' line1 col1 line2 col2 <-
  Span
    { start = Pos {line = line1, col = col1},
      end = Pos {line = line2, col = col2}
    }

{-# COMPLETE SpanV' #-}

_start :: L.Getter Span Pos
_start = L.to start

_end :: L.Getter Span Pos
_end = L.to end

_line1 :: L.Getter Span Int
_line1 = _start . #line

_line2 :: L.Getter Span Int
_line2 = _end . #line

_col1 :: L.Getter Span Int
_col1 = _start . #col

_col2 :: L.Getter Span Int
_col2 = _end . #col

-- | Does the first span contain the other?
contains :: Span -> Span -> Bool
contains (SpanV' line1 col1 line2 col2) (SpanV' line1' col1' line2' col2') =
  line1 <= line1' && line2' <= line2 && col1 <= col1' && col2' <= col2

-- this is unsafe because SourcePos is usually Char based
-- we want utf-16 code units
-- this is here so I can figure it out later
unsafeFromSourceRange :: (SourcePos, SourcePos) -> Span
unsafeFromSourceRange (pos1, pos2) = Span {start = Pos.unsafeFromSourcePos pos1, end = Pos.unsafeFromSourcePos pos2}

span :: L.Iso' LSP.Range Span
span =
  L.iso
    (\LSP.Range {_start, _end} -> Span {start = _start ^. Pos.position, end = _end ^. Pos.position})
    (\Span {start, end} -> LSP.Range {_start = start ^. L.from Pos.position, _end = end ^. L.from Pos.position})