module Handlers.Utils where

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
import UnliftIO (async)
import qualified UnliftIO.Exception as Exception
import qualified UnliftIO.IORef as IORef

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
    env <- Server.getLspEnv
    let act = fn msg
    void $
      async $ do
        stRef <- IORef.newIORef st
        res <-
          State.runServer stRef env act & Exception.tryAny
            >>= \case
              Left e -> do
                errorM "handlers" ("An exception occured inside of a request handler: " ++ Text.Show.show e)
                pure $
                  Left $
                    LSP.ResponseError
                      { _code = LSP.UnknownErrorCode,
                        _message = T.pack $ Text.Show.show e,
                        _xdata = Nothing
                      }
              Right x -> pure $ Right x
        State.runServer stRef env (k res)

getNoteId ::
  ( LSP.HasTextDocument a b,
    LSP.HasUri b LSP.Uri,
    MonadState State.ServerState m
  ) =>
  a ->
  m Int
getNoteId params = do
  let path = params ^. LSP.textDocument . LSP.uri . L.to (Unsafe.fromJust . Proto.uriToNormalizedFilePath)
  L.use (#pathToNote . L.at path . L.to Unsafe.fromJust)

getNote ::
  ( LSP.HasTextDocument a b,
    LSP.HasUri b LSP.Uri,
    MonadState State.ServerState m
  ) =>
  a ->
  m Note
getNote params = do
  noteId <- getNoteId params
  gets $ State.getNote noteId