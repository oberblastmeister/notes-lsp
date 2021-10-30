module State where

import Commonmark (ParseError)
import Config (Config, LanguageContextEnv)
import qualified Control.Lens as L
import Control.Lens.Operators
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.Graph.Inductive as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IORef as IORef
import qualified Data.IntMap.Strict as IntMap
import Data.Rope.UTF16 (Rope)
import Language.LSP.Server (LspM)
import qualified Language.LSP.Server as Server
import qualified Language.LSP.Types as LSP
import qualified Markdown.AST as AST
import Markdown.Connection (Connection)
import MyPrelude
import Note (Note, getName, new)
import qualified Relude.Unsafe as Unsafe

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
    liftIO $ IORef.readIORef ref

  put a = do
    ref <- ask
    liftIO $ IORef.writeIORef ref a

runServer :: MonadIO m => IORef ServerState -> Config.LanguageContextEnv -> ServerM a -> m a
runServer st env m = unServer m & (`runReaderT` st) & Server.runLspT env & liftIO

runServer' :: MonadIO m => ServerState -> Config.LanguageContextEnv -> ServerM' a -> m a
runServer' st env m = runReaderT m st & Server.runLspT env & liftIO

toServer' :: ServerM a -> ServerM' a
toServer' m = do
  st <- ask
  stRef <- liftIO $ IORef.newIORef st
  env <- Server.getLspEnv
  runServer stRef env m

data ServerState = ServerState
  { pathToNote :: HashMap LSP.NormalizedFilePath Int,
    nameToNote :: HashMap Text Int,
    notes :: IntMap Note,
    noteGraph :: Gr () Connection,
    size :: !Int
  }
  deriving (Show, Generic)

def :: ServerState
def =
  ServerState
    { pathToNote = HashMap.empty,
      nameToNote = HashMap.empty,
      notes = IntMap.empty,
      noteGraph = Graph.empty,
      size = 0
    }

addNote :: MonadState ServerState m => Note -> m Int
addNote note = do
  newId <- L.use #size
  #notes . L.at newId ?= note
  #size += 1
  pure newId

-- newtype NoteId = NoteId {unNoteId :: Int} deriving (Show, Eq, Ord)

getNote :: (MonadState ServerState m) => Int -> m Note
getNote noteId = L.use (#notes . L.at noteId . L.to Unsafe.fromJust)

addPath ::
  (MonadState ServerState m, MonadError ParseError m) =>
  LSP.NormalizedFilePath ->
  Text ->
  Rope ->
  m Int
addPath nPath text rope = do
  let path = LSP.fromNormalizedFilePath nPath
  let name = Note.getName path
  newNote <- Note.new nPath text rope
  maybeId <- L.use (#pathToNote . L.at nPath)
  noteId <- case maybeId of
    Nothing -> addNote newNote
    Just it -> do
      #notes . L.at it ?= newNote
      pure it
  #nameToNote . L.at name ?= noteId
  #pathToNote . L.at nPath ?= noteId
  #noteGraph %= Graph.insNode (noteId, ())
  pure noteId

changePath ::
  (MonadState ServerState m, MonadError ParseError m) =>
  LSP.NormalizedFilePath ->
  Text ->
  Rope ->
  m ()
changePath nPath text rope = do
  noteId <- addPath nPath text rope
  updateGraph noteId

updateGraph :: (MonadState ServerState m) => Int -> m ()
updateGraph noteId = do
  note <- getNote noteId
  let name = note ^. #name
  forM_ (note ^. #ast) $ \(astElement, _sp) -> case astElement of
    AST.LinkElement link -> do
      whenJustM
        (L.use (#nameToNote . L.at name))
        ( \toId -> do
            #noteGraph %= Graph.insEdge (noteId, toId, link ^. #conn)
        )
    _ -> pure ()