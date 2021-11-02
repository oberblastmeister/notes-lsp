module State where

import Commonmark (ParseError)
import Config (Config, LanguageContextEnv)
import qualified Control.Lens as L
import Control.Lens.Operators
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.Graph.Inductive as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.HashMap.Strict as HashMap
import qualified UnliftIO.IORef as IORef
import qualified Data.IntMap.Strict as IntMap
import Data.Rope.UTF16 (Rope)
import qualified Data.Text as T
import Language.LSP.Server (LspM)
import qualified Language.LSP.Server as Server
import qualified Language.LSP.Types as LSP
import LineIndex (LineIndex, new)
import Markdown.AST (AST)
import qualified Markdown.AST as AST
import Markdown.Connection (Connection)
import qualified Markdown.Parsing
import MyPrelude
import qualified Relude.Unsafe as Unsafe
import qualified System.FilePath as FilePath
import qualified Text.Show

newtype ServerM a = ServerM {unServer :: ReaderT (IORef ServerState) (LspM Config) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (IORef ServerState),
      MonadIO,
      MonadUnliftIO,
      Server.MonadLsp Config
    )

type MonadServer :: (* -> *) -> Constraint

type MonadServer m =
  ( MonadReader (IORef ServerState) m,
    MonadState ServerState m,
    MonadIO m
  )

type ServerM' = ReaderT ServerState (LspM Config)

instance MonadState ServerState ServerM where
  get = do
    ref <- ask
    IORef.readIORef ref

  put a = do
    ref <- ask
    IORef.writeIORef ref a

runServer :: MonadIO m => IORef ServerState -> Config.LanguageContextEnv -> ServerM a -> m a
runServer st env m = unServer m & (`runReaderT` st) & Server.runLspT env & liftIO

runServer' :: MonadIO m => ServerState -> Config.LanguageContextEnv -> ServerM' a -> m a
runServer' st env m = runReaderT m st & Server.runLspT env & liftIO

toServer' :: ServerM a -> ServerM' a
toServer' m = do
  st <- ask
  stRef <- IORef.newIORef st
  env <- Server.getLspEnv
  runServer stRef env m

data ServerState = ServerState
  { pathToNote :: HashMap LSP.NormalizedFilePath Int,
    nameToNote :: HashMap Text Int,
    notes :: IntMap Note,
    noteGraph :: Gr () Connection,
    -- amount of notes
    size :: !Int
  }
  deriving (Generic)

instance Show ServerState where
  show ServerState {pathToNote, nameToNote, noteGraph} =
    Text.Show.show pathToNote
      ++ Text.Show.show nameToNote
      ++ Text.Show.show noteGraph

def :: ServerState
def =
  ServerState
    { pathToNote = HashMap.empty,
      nameToNote = HashMap.empty,
      notes = IntMap.empty,
      noteGraph = Graph.empty,
      size = 0
    }

combine :: ServerState -> ServerState -> ServerState
combine st st' = do
  execState
    ( do
        noteIds <- forM (st' ^.. #notes . L.each) updateNote
        forM_ noteIds updateNoteGraph
    )
    st

data Note = Note
  { ast :: AST,
    rope :: Rope,
    name :: Text,
    path :: LSP.NormalizedFilePath,
    lineIndex :: LineIndex,
    id :: Int
  }
  deriving (Show, Generic)

updateNote :: MonadState ServerState m => Note -> m Int
updateNote note = do
  let nPath = note ^. #path
      name = note ^. #name
  maybeId <- L.use (#pathToNote . L.at nPath)
  noteId <- case maybeId of
    Nothing -> do
      newId <- L.use #size
      #notes . L.at newId ?= (note & #id .~ newId)
      #size += 1
      pure newId
    Just it -> do
      #notes . L.at it ?= (note & #id .~ it)
      pure it
  #nameToNote . L.at name ?= noteId
  #pathToNote . L.at nPath ?= noteId
  #noteGraph %= Graph.insNode (noteId, ())
  pure noteId

getNote ::  Int -> ServerState -> Note
getNote noteId = L.view (#notes . L.at noteId . L.to Unsafe.fromJust)

getName :: FilePath -> Text
getName = T.pack . FilePath.dropExtension . FilePath.takeFileName

newNote ::
  (MonadState ServerState m, MonadError ParseError m) =>
  LSP.NormalizedFilePath ->
  Text ->
  Rope ->
  m Int
newNote nPath text rope = do
  let path = LSP.fromNormalizedFilePath nPath
      name = getName path
      lineIndex = LineIndex.new text
  ast <- liftEither $ Markdown.Parsing.parseAST path text
  let note = Note {ast, rope, name, path = nPath, lineIndex, id = error "id not initialized yet"}
  updateNote note

changeNote ::
  (MonadState ServerState m, MonadError ParseError m) =>
  LSP.NormalizedFilePath ->
  Text ->
  Rope ->
  m ()
changeNote nPath text rope = do
  noteId <- newNote nPath text rope
  updateNoteGraph noteId

updateNoteGraph :: (MonadState ServerState m) => Int -> m ()
updateNoteGraph noteId = do
  note <- gets $ getNote noteId
  forM_ (note ^. #ast) $ \(astElement, _sp) -> case astElement of
    AST.LinkElement link -> do
      whenJustM
        (L.use (#nameToNote . L.at (link ^. #dest)))
        ( \toId -> do
            #noteGraph %= Graph.insEdge (noteId, toId, link ^. #conn)
        )
    _ -> pure ()