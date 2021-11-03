module Proto
  ( rowColumn,
    position,
    uriToNormalizedFilePath,
    normalizedFilePathToUri,
    topLocation,
    normalizedUri,
  )
where

import qualified Commonmark
import Control.Lens
import qualified Control.Lens as L
import Data.Pos (Pos (Pos))
import qualified Data.Rope.UTF16 as Rope
import qualified Data.Span as Span
import qualified Language.LSP.Types as LSP
import MyPrelude
import qualified Text.Parsec.Pos as Parsec.Pos
import qualified Language.LSP.Types.Lens as LSP

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

uriToNormalizedFilePath :: LSP.Uri -> Maybe LSP.NormalizedFilePath
uriToNormalizedFilePath = LSP.uriToNormalizedFilePath . LSP.toNormalizedUri

normalizedFilePathToUri :: LSP.NormalizedFilePath -> LSP.Uri
normalizedFilePathToUri = LSP.fromNormalizedUri . LSP.normalizedFilePathToUri

topLocation :: LSP.Uri -> LSP.Location
topLocation = flip LSP.Location (Span.empty (Pos 0 0) ^. L.from Span.span)

normalizedUri :: L.Getter LSP.TextDocumentItem LSP.NormalizedUri
normalizedUri = LSP.uri . to LSP.toNormalizedUri