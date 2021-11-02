module Data.Pos
  ( Pos (..),
    unsafeFromSourcePos,
    position,
  )
where

import Commonmark
import MyPrelude
import Text.Parsec.Pos (sourceColumn, sourceLine)
import qualified Control.Lens as L
import qualified Language.LSP.Types as LSP

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