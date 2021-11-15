module NoteData where

import qualified Control.Lens as L
import Control.Lens.Operators
import qualified Data.Graph.Inductive as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.IdMap (Id, IdMap, atId, unId)
import qualified Language.LSP.Types as LSP
import qualified Markdown.AST as AST
import Markdown.Connection (Connection)
import MyPrelude
import Note (Note)

type NoteId = Id Note

type NoteGraph = Gr () Connection

data NoteData = NoteData
  { pathToNote :: HashMap LSP.NormalizedFilePath NoteId,
    nameToNote :: HashMap Text (Set NoteId),
    notes :: IdMap Note,
    noteGraph :: Gr () Connection
  }
  deriving (Generic)

-- updateGraph :: NoteId -> NoteData -> NoteData
-- updateGraph noteId noteData = noteData & #noteGraph %~ updateGraph'
--   where
--     note = noteData ^. #notes . atId noteId

--     elems = note ^. #ast . #elems & map fst

--     updateGraph' :: NoteGraph -> NoteGraph
--     updateGraph' graph =
--       foldl'
--         ( \gr -> \case
--             AST.LinkElement link | L.has (#nameToNote . L.at (link ^. #dest) . L.ix noteId) noteData -> do
--               Graph.insEdge (unId noteId, unId toId, link ^. #conn) gr
--             _ -> gr
--         )
--         graph
--         elems