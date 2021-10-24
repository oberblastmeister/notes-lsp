module Proto
  ( rowColumn,
  )
where

import Control.Lens
import qualified Data.Rope.UTF16 as Rope
import qualified Language.LSP.Types as LSP

rowColumn :: Iso' LSP.Position Rope.RowColumn
rowColumn =
  iso
    (\(LSP.Position row column) -> Rope.RowColumn {row, column})
    (\(Rope.RowColumn _line _character) -> LSP.Position {_line, _character})