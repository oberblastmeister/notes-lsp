module Data.Pos
  ( Pos (..),
    unsafeFromSourcePos,
    position,
    rowColumn,
  )
where

import Commonmark
import qualified Control.Lens as L
import qualified Data.Rope.UTF16 as Rope
import qualified Language.LSP.Types as LSP
import MyPrelude
import Text.Parsec.Pos (sourceColumn, sourceLine)

data Pos = Pos
  { line :: !Int,
    col :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

unsafeFromSourcePos :: SourcePos -> Pos
unsafeFromSourcePos spos = Pos {line = sourceLine spos - 1, col = sourceColumn spos - 1}

position :: L.Iso' LSP.Position Pos
position =
  L.iso
    (\LSP.Position {_line, _character} -> Pos {line = _line, col = _character})
    (\Pos {line, col} -> LSP.Position {_line = line, _character = col})

rowColumn :: L.Iso' Rope.RowColumn Pos
rowColumn =
  L.iso
    (\Rope.RowColumn {row, column} -> Pos {line = row, col = column})
    (\Pos {line, col} -> Rope.RowColumn {row = line, column = col})