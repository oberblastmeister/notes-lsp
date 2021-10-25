module Proto
  ( rowColumn,
    position,
  )
where

import qualified Commonmark
import Control.Lens
import qualified Data.Rope.UTF16 as Rope
import qualified Language.LSP.Types as LSP
import MyPrelude
import qualified Text.Parsec.Pos as Parsec.Pos

rowColumn :: Iso' LSP.Position Rope.RowColumn
rowColumn =
  iso
    (\LSP.Position {_line = row, _character = column} -> Rope.RowColumn {row, column})
    (\Rope.RowColumn {row = _line, column = _character} -> LSP.Position {_line, _character})

position :: Iso' Commonmark.SourcePos LSP.Position
position =
  iso
    (\pos -> LSP.Position (Parsec.Pos.sourceLine pos - 1) (Parsec.Pos.sourceColumn pos - 1))
    (\LSP.Position {_line, _character} -> Parsec.Pos.newPos "<none>" (_line + 1) (_character + 1))