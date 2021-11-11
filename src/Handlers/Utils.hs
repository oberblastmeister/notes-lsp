-- module Handlers.Utils where

-- import qualified Control.Lens as L
-- import Control.Lens.Operators
-- import qualified Data.Text as T
-- import qualified Language.LSP.Server as Server
-- import qualified Language.LSP.Types as LSP
-- import qualified Language.LSP.Types.Lens as LSP
-- import Logging
-- import MyPrelude
-- import qualified Proto
-- import qualified Relude.Unsafe as Unsafe
-- import State (Note, ServerM)
-- import qualified State
-- import qualified Text.Show
-- import UnliftIO.Exception (evaluate)
-- import qualified UnliftIO.Exception as Exception
-- import qualified Utils

-- type Handler a = Server.Handler ServerM a

-- type Handlers = Server.Handlers ServerM

-- type family HandlerReturn f m where
--   HandlerReturn f (m :: LSP.Method _from 'LSP.Request) = LSP.RequestMessage m -> f (LSP.ResponseResult m)

-- notificationHandler ::
--   forall (m :: LSP.Method 'LSP.FromClient 'LSP.Notification).
--   LSP.SMethod m ->
--   Server.Handler ServerM m ->
--   Server.Handlers ServerM
-- notificationHandler smethod fn = do
--   Server.notificationHandler smethod $ \msg -> do
--     fn msg & Exception.tryAny >>= \case
--       Left e -> do
--         errorM "handlers" ("An exception occured inside of a notification handler " ++ Text.Show.show e)
--       Right () -> pure ()

-- requestHandler ::
--   forall (m :: LSP.Method 'LSP.FromClient 'LSP.Request).
--   LSP.SMethod m ->
--   HandlerReturn ServerM m ->
--   Server.Handlers ServerM
-- requestHandler smethod fn = do
--   Server.requestHandler smethod $ \msg k -> do
--     st <- get
--     let act = fn msg
--     void $
--       -- don't swallow exceptions
--       Utils.async' $ do
--         -- State.forkState st $ do
--         --   void $ act & errorToResponse k <&> traverse (k . Right)

--         -- important so we don't modify IORef from different threads
--         State.forkState st $ do
--           !res <-
--             act & Exception.tryAny
--               >>= \case
--                 Left e -> do
--                   errorM "handlers" ("An exception occured inside of a request handler: " ++ Text.Show.show e)
--                   pure $
--                     Left $
--                       LSP.ResponseError
--                         { _code = LSP.InternalError,
--                           _message = T.pack $ Text.Show.show e,
--                           _xdata = Nothing
--                         }
--                 Right !x -> pure $ Right x
--           k res & Exception.tryAny >>= print

-- newResponseError :: Text -> LSP.ResponseError
-- newResponseError t =
--   LSP.ResponseError
--     { _code = LSP.InternalError,
--       _message = t,
--       _xdata = Nothing
--     }

-- errorToResponse :: State.Responder r -> ServerM a -> ServerM (Maybe a)
-- errorToResponse responder act =
--   Exception.tryAny act >>= \case
--     Left e -> do
--       responder $ Left $ newResponseError $ T.pack $ Text.Show.show e
--       pure Nothing
--     Right x -> do
--       -- important to get exceptions before they are sent to the lsp thread
--       -- where they will be ignored
--       void $ evaluate x
--       pure $ Just x

-- getNoteId ::
--   ( LSP.HasTextDocument a b,
--     LSP.HasUri b LSP.Uri,
--     MonadState State.ServerState m,
--     MonadIO m
--   ) =>
--   a ->
--   m Int
-- getNoteId params = do
--   let uri = params ^. LSP.textDocument . LSP.uri
--   let mpath = Proto.uriToNormalizedFilePath uri
--   path <- whenNothing mpath $ do
--     Exception.throwString $ "BUG: Could not convert uri " ++ show uri ++ " to a normalized path"
--   pathToNote <- L.use #pathToNote
--   whenNothing (pathToNote ^. L.at path) $ do
--     Exception.throwString $ "BUG: pathToNote: " ++ show pathToNote ++ "Could not find path: " ++ show path

-- getNote ::
--   ( LSP.HasTextDocument a b,
--     LSP.HasUri b LSP.Uri,
--     MonadState State.ServerState m,
--     MonadIO m
--   ) =>
--   a ->
--   m Note
-- getNote params = do
--   noteId <- getNoteId params
--   gets $ State.getNote noteId

module Handlers.Utils where

import Control.Concurrent (forkIO)
import qualified Control.Lens as L
import Control.Lens.Operators
import qualified Data.Text as T
import qualified Language.LSP.Server as Server
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import Logging
import MyPrelude
import qualified Proto
import qualified Relude.Unsafe as Unsafe
import State (Note, ServerM)
import qualified State
import qualified Text.Show
import UnliftIO (async, wait)
import qualified UnliftIO.Exception as Exception
import qualified UnliftIO.IORef as IORef
import qualified Utils

type Handler a = Server.Handler ServerM a

type Handlers = Server.Handlers ServerM

type family HandlerReturn f m where
  HandlerReturn f (m :: LSP.Method _from 'LSP.Request) = LSP.RequestMessage m -> f (LSP.ResponseResult m)

notificationHandler ::
  forall (m :: LSP.Method 'LSP.FromClient 'LSP.Notification).
  LSP.SMethod m ->
  Server.Handler ServerM m ->
  Server.Handlers ServerM
notificationHandler smethod fn = do
  Server.notificationHandler smethod $ \msg -> do
    fn msg & Exception.tryAny >>= \case
      Left e -> do
        errorM "handlers" ("An exception occured inside of a notification handler " ++ Text.Show.show e)
      Right () -> pure ()

requestHandler ::
  forall (m :: LSP.Method 'LSP.FromClient 'LSP.Request).
  LSP.SMethod m ->
  HandlerReturn ServerM m ->
  Server.Handlers ServerM
requestHandler smethod fn = do
  Server.requestHandler smethod $ \msg k -> do
    st <- get
    let act = fn msg
    void $
      -- don't swallow exceptions
      Utils.async' $ do
        -- important so we don't modify IORef from different threads
        State.forkState st $ do
          res <-
            act & Exception.tryAny
              >>= \case
                Left e -> do
                  errorM "handlers" ("An exception occured inside of a request handler: " ++ Text.Show.show e)
                  pure $ Left $ newResponseError $ T.pack $ Text.Show.show e
                Right x -> do
                  Exception.evaluate x
                  pure $ Right x
          k res

newResponseError :: Text -> LSP.ResponseError
newResponseError t =
  LSP.ResponseError
    { _code = LSP.InternalError,
      _message = t,
      _xdata = Nothing
    }

getNoteId ::
  ( LSP.HasTextDocument a b,
    LSP.HasUri b LSP.Uri,
    MonadState State.ServerState m,
    MonadIO m
  ) =>
  a ->
  m Int
getNoteId params = do
  let uri = params ^. LSP.textDocument . LSP.uri
  let mpath = Proto.uriToNormalizedFilePath uri
  path <- whenNothing mpath $ do
    Exception.throwString $ "BUG: Could not convert uri " ++ show uri ++ " to a normalized path"
  pathToNote <- L.use #pathToNote
  whenNothing (pathToNote ^. L.at path) $ do
    Exception.throwString $ "BUG: pathToNote: " ++ show pathToNote ++ "Could not find path: " ++ show path

getNote ::
  ( LSP.HasTextDocument a b,
    LSP.HasUri b LSP.Uri,
    MonadState State.ServerState m,
    MonadIO m
  ) =>
  a ->
  m Note
getNote params = do
  noteId <- getNoteId params
  gets $ State.getNote noteId