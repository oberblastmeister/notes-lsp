module State where

import MyPrelude
import Config (Config, LanguageContextEnv)
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
  { uriToNote :: HashMap LSP.NormalizedUri NoteId,
    notes :: IntMap Note,
    noteGraph :: Gr NoteId LinkData
  }
  deriving (Show, Generic)

def :: ServerState
def =
  ServerState
    { uriToNote = HashMap.empty,
      notes = IntMap.empty,
      noteGraph = Graph.empty
    }

newtype NoteId = NoteIt {unNoteId :: Int} deriving (Show)

data LinkData = LinkData
  deriving (Show, Generic)

data Note = Note
  { tree :: (),
    contents :: Rope
  }
  deriving (Show, Generic)