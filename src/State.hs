module State
  ( ServerM,
    MonadServer,
    runServer,
    ServerState,
    LinkKind (..),
    def,
    updateNote,
    getNote,
    getName,
    newNote,
    applyNewRope,
    updateNoteGraph,
    Note (..),
    Responder,
    forkState,
    combine,
  )
where

import Config (Config, LanguageContextEnv)
import qualified Control.Lens as L
import Control.Lens.Operators
import Control.Monad.Except (MonadError, liftEither)
import qualified Data.Graph.Inductive as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap.Strict as IntMap
import Data.Rope.UTF16 (Rope)
import qualified Data.Rope.UTF16 as Rope
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
import qualified UnliftIO.IORef as IORef

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
    MonadUnliftIO m,
    Server.MonadLsp Config m
  )

instance MonadState ServerState ServerM where
  get = do
    ref <- ask
    IORef.readIORef ref

  put a = do
    ref <- ask
    IORef.writeIORef ref a

type Responder r = Either LSP.ResponseError r -> ServerM ()

runServer :: MonadIO m => IORef ServerState -> Config.LanguageContextEnv -> ServerM a -> m a
runServer st env m = unServer m & (`runReaderT` st) & Server.runLspT env & liftIO

-- | Seed the monad with a state
-- | The monad will not actually modify the resulting state,
-- | the seed state is just to run the monad
forkState :: ServerState -> ServerM a -> ServerM a
forkState st m = do
  env <- Server.getLspEnv
  stRef <- IORef.newIORef st
  runServer stRef env m

-- -- this code is very brittle
-- requestHandler ::
--   forall (m :: LSP.Method 'LSP.FromClient 'LSP.Request).
--   LSP.SMethod m ->
--   HandlerReturn ServerM m ->
--   Server.Handlers ServerM
-- requestHandler smethod fn = do
--   Server.requestHandler smethod $ \msg k -> do
--     st <- get
--     let act = fn msg
--     env <- Server.getLspEnv
--     void $
--       async $ do
--         stRef <- IORef.newIORef st
--         !res <-
--           State.runServer stRef env act & Exception.tryAny
--             >>= \case
--               Left e -> do
--                 errorM "handlers" ("An exception occured inside of a request handler: " ++ Text.Show.show e)
--                 pure $
--                   Left $
--                     LSP.ResponseError
--                       { _code = LSP.UnknownErrorCode,
--                         _message = T.pack $ Text.Show.show e,
--                         _xdata = Nothing
--                       }
--               Right x -> pure $ Right x
--         State.runServer stRef env (k res)

-- | Used to prefer the first ServerState when combine two ServerState
-- | Useful when one server state is calculated concurrently, but the current ServerState
-- | Is the one we have already
combine :: ServerState -> ServerState -> ServerState
combine st st' = do
  flip execState st $ do
    noteIds <- forM (st' ^.. #notes . L.each) updateNote
    forM_ noteIds updateNoteGraph

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

instance Default ServerState where
  def =
    ServerState
      { pathToNote = HashMap.empty,
        nameToNote = HashMap.empty,
        notes = IntMap.empty,
        noteGraph = Graph.empty,
        size = 0
      }

data Note = Note
  { ast :: AST,
    rope :: Rope,
    name :: Text,
    path :: LSP.NormalizedFilePath,
    lineIndex :: LineIndex
  }
  deriving (Show, Generic)

data LinkKind = Normal | Folgezettel
  deriving (Show, Eq, Generic)

-- combine :: ServerState -> ServerState -> [Note]
-- combine st st' = st ^.. #notes . L.each

updateNote :: MonadState ServerState m => Note -> m Int
updateNote note = do
  let nPath = note ^. #path
      name = note ^. #name
  maybeId <- L.use (#pathToNote . L.at nPath)
  case maybeId of
    Nothing -> do
      newId <- L.use #size
      #notes . L.at newId ?= note
      #size += 1
      #nameToNote . L.at name ?= newId
      #pathToNote . L.at nPath ?= newId
      #noteGraph %= Graph.insNode (newId, ())
      pure newId
    Just it -> do
      #notes . L.at it ?= note
      pure it

getNote :: Int -> ServerState -> Note
getNote noteId = L.view (#notes . L.at noteId . L.to Unsafe.fromJust)

getName :: FilePath -> Text
getName = T.pack . FilePath.dropExtension . FilePath.takeFileName

newNote ::
  (MonadState ServerState m, MonadError Text m) =>
  LSP.NormalizedFilePath ->
  Rope ->
  m Int
newNote nPath rope = do
  let path = LSP.fromNormalizedFilePath nPath
      text = Rope.toText rope
      name = getName path
      lineIndex = LineIndex.new text
  ast <- liftEither $ Markdown.Parsing.parseAST path text
  let note = Note {ast, rope, name, path = nPath, lineIndex}
  updateNote note

applyNewRope ::
  (MonadState ServerState m, MonadError Text m) =>
  LSP.NormalizedFilePath ->
  Rope ->
  m ()
applyNewRope nPath rope = do
  noteId <- newNote nPath rope
  updateNoteGraph noteId

updateNoteGraph :: (MonadState ServerState m) => Int -> m ()
updateNoteGraph noteId = do
  note <- gets $ getNote noteId
  forM_ (note ^. #ast . #elems) $ \(astElement, _sp) -> case astElement of
    AST.LinkElement link -> do
      whenJustM
        -- this should be find dest or something
        (L.use (#nameToNote . L.at (link ^. #dest)))
        ( \toId -> do
            #noteGraph %= Graph.insEdge (noteId, toId, link ^. #conn)
        )
    _ -> pure ()