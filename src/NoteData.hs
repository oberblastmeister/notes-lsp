module NoteData where

import qualified Control.Lens as L
import Control.Lens.Operators
import qualified Data.Graph.Inductive as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.IdMap (Id, IdMap, atId, unId)
import qualified Data.Set as Set
import qualified Language.LSP.Types as LSP
import LinkPath (WikiLink)
import qualified Markdown.AST as AST
import Markdown.Connection (Connection)
import MyPrelude
import Note (Note)

type NoteId = Id Note

type NoteGraph = Gr () Connection

data NoteData = NoteData
  { pathToNote :: HashMap LSP.NormalizedFilePath NoteId,
    nameToNote :: HashMap Text (Set NoteId),
    wikiLinkToNote :: HashMap WikiLink (Set NoteId),
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
--             AST.LinkElement link -> do
--               Graph.insEdge (unId noteId, unId toId, link ^. #conn) gr
--             _ -> gr
--         )
--         graph
--         elems

data ResolveError
  = ResolveNotFound WikiLink
  | ResolveAmbiguous (NonEmpty Note)
  deriving (Show, Eq)

linkTarget :: WikiLink -> NoteData -> Either ResolveError NoteId
linkTarget link noteData =
  case nonEmpty candidates of
    Nothing -> Left $ ResolveNotFound link
    Just (x :| []) -> Right x
    Just other -> Left $ ResolveAmbiguous (fmap (\nid -> noteData ^. #notes . atId nid) other)
  where
    candidates = noteData ^.. #wikiLinkToNote . L.ix link . L.to Set.elems . L.each