{-# LANGUAGE QuasiQuotes #-}

-- |
-- This is an example language server built with haskell-lsp using a 'Reactor'
-- design. With a 'Reactor' all requests are handled on a /single thread/.
-- A thread is spun up for it, which repeatedly reads from a 'TChan' of
-- 'ReactorInput's.
-- The `lsp` handlers then simply pass on all the requests and
-- notifications onto the channel via 'ReactorInput's.
-- This way there is the option of executing requests on multiple threads, without
-- blocking server communication.
-- To try out this server, install it with
-- > cabal install lsp-demo-reactor-server -fdemo
-- and plug it into your client of choice.
module Reactor (main) where

import qualified Config
import qualified Control.Concurrent as Concurrent
-- import qualified Control.Monad.STM as STM

-- import qualified Control.Monad.STM as STM
import Control.Concurrent.STM (TQueue)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import Control.Lens
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.IORef as IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Language.LSP.Diagnostics as Diagnostics
import qualified Language.LSP.Server as Server
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified Language.LSP.VFS as VFS
import MyPrelude
import Path (Dir, File, Rel, toFilePath, (</>))
import qualified Path
import qualified Path.IO as PIO
import State (ServerM, ServerM', ServerState)
import qualified State
import qualified System.Exit as Exit
import System.Log.Logger (debugM, errorM)
import qualified System.Log.Logger as Logger

main :: IO ()
main = do
  run >>= \case
    0 -> Exit.exitSuccess
    c -> Exit.exitWith . Exit.ExitFailure $ c

run :: IO Int
run = flip E.catches handlers $ do
  rChan <- STM.atomically STM.newTQueue
  stChan <- STM.atomically STM.newTQueue
  defSt <- IORef.newIORef State.def

  let serverDefinition =
        Server.ServerDefinition
          { defaultConfig = Config.def,
            onConfigurationChange = \_old v -> do
              case Aeson.fromJSON v of
                Aeson.Error e -> Left (T.pack e)
                Aeson.Success cfg -> Right cfg,
            doInitialize = \env _ -> do
              Concurrent.forkIO $ initializeState stChan
              Concurrent.forkIO $ runReaderT (reactor stChan rChan) defSt
              pure $ Right env,
            staticHandlers = lspHandlers rChan,
            interpretHandler = \env -> Server.Iso (State.runServer defSt env) liftIO,
            options = lspOptions
          }

  flip E.finally finalProc $ do
    logDir <- PIO.getXdgDir PIO.XdgData $ Just [Path.reldir|notes-lsp|]
    PIO.ensureDir logDir
    let logFile = logDir </> [Path.relfile|notes-lsp.log|]
    unlessM (PIO.doesFileExist logFile) $ TIO.writeFile (toFilePath logFile) ""
    Server.setupLogger (Just $ toFilePath logFile) ["reactor"] Logger.DEBUG
    Server.runServer serverDefinition
  where
    handlers =
      [ E.Handler ioExcept,
        E.Handler someExcept
      ]
    finalProc = Logger.removeAllHandlers
    ioExcept (e :: E.IOException) = print e >> return 1
    someExcept (e :: E.SomeException) = print e >> return 1

syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { LSP._openClose = Just True,
      LSP._change = Just LSP.TdSyncIncremental,
      LSP._willSave = Just False,
      LSP._willSaveWaitUntil = Just False,
      LSP._save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
    }

lspOptions :: Server.Options
lspOptions =
  Server.defaultOptions
    { Server.textDocumentSync = Just syncOptions,
      Server.executeCommandCommands = Just ["lsp-hello-command"]
    }

initializeState :: STM.TQueue ServerState -> IO ()
initializeState stChan = do
  return ()

-- ---------------------------------------------------------------------

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

data ReactorMsg
  = ReactorMsgAct ReactorAct
  | ReactorMsgInitState ServerState

data ReactorAct = ReactorAct
  { act :: Either (ServerM ()) (ServerM' ()),
    env :: Config.LanguageContextEnv
  }

select :: MonadIO m => [STM a] -> m a
select = liftIO . STM.atomically . Monad.msum

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: Config.MonadLsp m => LSP.NormalizedUri -> Maybe Int -> m ()
sendDiagnostics fileUri version = do
  let diags =
        [ LSP.Diagnostic
            (LSP.Range (LSP.Position 0 1) (LSP.Position 0 5))
            (Just LSP.DsWarning) -- severity
            Nothing -- code
            (Just "lsp-hello") -- source
            "Example diagnostic message"
            Nothing -- tags
            (Just (LSP.List []))
        ]
  Server.publishDiagnostics 100 fileUri version (Diagnostics.partitionBySource diags)

type MonadReactor :: (* -> *) -> Constraint

type MonadReactor m = (MonadReader (IORef ServerState) m, MonadIO m)

type ReactorM = ReaderT (IORef ServerState) IO ()

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: TQueue ServerState -> TQueue ReactorAct -> ReactorM
reactor = reactor'

reactor' :: (MonadReactor m) => TQueue ServerState -> TQueue ReactorAct -> m ()
reactor' stChan rChan = do
  liftIO $ debugM "reactor" "Started the reactor"
  forever $ do
    msg <-
      select
        [ STM.readTQueue rChan <&> ReactorMsgAct,
          STM.readTQueue stChan <&> ReactorMsgInitState
        ]
    case msg of
      ReactorMsgAct ReactorAct {act, env} -> do
        case act of
          Left act' -> do
            st <- ask
            State.runServer st env act'
          Right act' -> do
            undefined
      ReactorMsgInitState newSt -> do
        st <- ask
        liftIO $ IORef.writeIORef st newSt

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: TQueue ReactorAct -> Server.Handlers ServerM
lspHandlers rin = Server.mapHandlers goReq goNot handle
  where
    send = liftIO . STM.atomically . STM.writeTQueue rin

    goReq :: forall (a :: LSP.Method 'LSP.FromClient 'LSP.Request). Server.Handler ServerM a -> Server.Handler ServerM a
    goReq f = \msg k -> do
      env <- Server.getLspEnv
      send ReactorAct {act = Left $ f msg k, env}

    goNot :: forall (a :: LSP.Method 'LSP.FromClient 'LSP.Notification). Server.Handler ServerM a -> Server.Handler ServerM a
    goNot f = \msg -> do
      env <- Server.getLspEnv
      send ReactorAct {act = Left $ f msg, env}

-- | Where the actual logic resides for handling requests and notifications.
handle :: Server.Handlers ServerM
handle =
  mconcat
    [ notificationHandler LSP.SInitialized $ \_msg -> do
        liftIO $ debugM "reactor.handle" "Processing the Initialized notification"

        -- We're initialized! Lets send a showMessageRequest now
        let params =
              LSP.ShowMessageRequestParams
                LSP.MtWarning
                "What's your favourite language extension?"
                (Just [LSP.MessageActionItem "Rank2Types", LSP.MessageActionItem "NPlusKPatterns"])

        void $
          Server.sendRequest LSP.SWindowShowMessageRequest params $ \case
            Left e -> liftIO $ errorM "reactor.handle" $ "Got an error: " ++ show e
            Right _ -> do
              Server.sendNotification LSP.SWindowShowMessage $ LSP.ShowMessageParams LSP.MtInfo "Excellent choice"

              -- We can dynamically register a capability once the user accepts it
              Server.sendNotification LSP.SWindowShowMessage $ LSP.ShowMessageParams LSP.MtInfo "Turning on code lenses dynamically"

              let regOpts = LSP.CodeLensRegistrationOptions Nothing Nothing (Just False)

              void $
                Server.registerCapability LSP.STextDocumentCodeLens regOpts $ \_req responder -> do
                  liftIO $ debugM "reactor.handle" "Processing a textDocument/codeLens request"
                  let cmd = LSP.Command "Say hello" "lsp-hello-command" Nothing
                      rsp = LSP.List [LSP.CodeLens (LSP.mkRange 0 0 0 100) (Just cmd) Nothing]
                  responder (Right rsp),
      notificationHandler LSP.STextDocumentDidOpen $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            fileName = LSP.uriToFilePath doc
        liftIO $ debugM "reactor.handle" $ "Processing DidOpenTextDocument for: " ++ show fileName
        sendDiagnostics (LSP.toNormalizedUri doc) (Just 0),
      notificationHandler LSP.SWorkspaceDidChangeConfiguration $ \msg -> do
        cfg <- Server.getConfig
        liftIO $ debugM "configuration changed: " (show (msg, cfg))
        Server.sendNotification LSP.SWindowShowMessage $
          LSP.ShowMessageParams LSP.MtInfo $ "Wibble factor set to " <> T.pack (show $ cfg ^. #wibbleFactor),
      notificationHandler LSP.STextDocumentDidChange $ \msg -> do
        let doc =
              msg
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to LSP.toNormalizedUri
        liftIO $ debugM "reactor.handle" $ "Processing DidChangeTextDocument for: " ++ show doc
        mdoc <- Server.getVirtualFile doc
        case mdoc of
          Just doc@(VFS.VirtualFile _version str _) -> do
            liftIO $ do
              debugM "reactor.handle" $ "Found the virtual file: " ++ show str
              debugM "reactor.handle" $ "Virtual file text: " ++ show (VFS.virtualFileText doc)
          Nothing -> do
            liftIO $ debugM "reactor.handle" $ "Didn't find anything in the VFS for: " ++ show doc,
      notificationHandler LSP.STextDocumentDidSave $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            fileName = LSP.uriToFilePath doc
        liftIO $ debugM "reactor.handle" $ "Processing DidSaveTextDocument  for: " ++ show fileName
        sendDiagnostics (LSP.toNormalizedUri doc) Nothing,
      requestHandler LSP.STextDocumentRename $ \req responder -> do
        liftIO $ debugM "reactor.handle" "Processing a textDocument/rename request"
        let params = req ^. LSP.params
            LSP.Position l c = params ^. LSP.position
            newName = params ^. LSP.newName
        vdoc <- Server.getVersionedTextDoc (params ^. LSP.textDocument)
        -- Replace some text at the position with what the user entered
        let edit = LSP.InL $ LSP.TextEdit (LSP.mkRange l c l (c + T.length newName)) newName
            tde = LSP.TextDocumentEdit vdoc (LSP.List [edit])
            -- "documentChanges" field is preferred over "changes"
            rsp = LSP.WorkspaceEdit Nothing (Just (LSP.List [LSP.InL tde])) Nothing
        responder (Right rsp),
      requestHandler LSP.STextDocumentHover $ \req responder -> do
        liftIO $ debugM "reactor.handle" "Processing a textDocument/hover request"
        let LSP.HoverParams _doc pos _workDone = req ^. LSP.params
            LSP.Position _l _c' = pos
            rsp = LSP.Hover ms (Just range)
            ms = LSP.HoverContents $ LSP.markedUpContent "lsp-hello" "Your type info here!"
            range = LSP.Range pos pos
        responder (Right $ Just rsp),
      requestHandler LSP.STextDocumentDocumentSymbol $ \req responder -> do
        liftIO $ debugM "reactor.handle" "Processing a textDocument/documentSymbol request"
        let LSP.DocumentSymbolParams _ _ doc = req ^. LSP.params
            loc = LSP.Location (doc ^. LSP.uri) (LSP.Range (LSP.Position 0 0) (LSP.Position 0 0))
            sym = LSP.SymbolInformation "lsp-hello" LSP.SkFunction Nothing Nothing loc Nothing
            rsp = LSP.InR (LSP.List [sym])
        responder (Right rsp),
      requestHandler LSP.STextDocumentCodeAction $ \req responder -> do
        liftIO $ debugM "reactor.handle" "Processing a textDocument/codeAction request"
        let params = req ^. LSP.params
            doc = params ^. LSP.textDocument
            (LSP.List diags) = params ^. LSP.context . LSP.diagnostics
            -- makeCommand only generates commands for diagnostics whose source is us
            makeCommand (LSP.Diagnostic (LSP.Range start _) _s _c (Just "lsp-hello") _m _t _l) = [LSP.Command title cmd cmdparams]
              where
                title = "Apply LSP hello command:" <> (T.lines _m ^?! _head)
                -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
                cmd = "lsp-hello-command"
                -- need 'file' and 'start_pos'
                args =
                  LSP.List
                    [ Aeson.Object $ H.fromList [("file", Aeson.Object $ H.fromList [("textDocument", Aeson.toJSON doc)])],
                      Aeson.Object $ H.fromList [("start_pos", Aeson.Object $ H.fromList [("position", Aeson.toJSON start)])]
                    ]
                cmdparams = Just args
            makeCommand (LSP.Diagnostic _r _s _c _source _m _t _l) = []
            rsp = LSP.List $ map LSP.InL $ concatMap makeCommand diags
        responder (Right rsp),
      requestHandler LSP.SWorkspaceExecuteCommand $ \req responder -> do
        liftIO $ debugM "reactor.handle" "Processing a workspace/executeCommand request"
        let params = req ^. LSP.params
            margs = params ^. LSP.arguments

        liftIO $ debugM "reactor.handle" $ "The arguments are: " ++ show margs
        responder (Right (Aeson.Object mempty)) -- respond to the request
        void $
          Server.withProgress "Executing some long running command" Server.Cancellable $ \update ->
            forM [(0 :: Double) .. 10] $ \i -> do
              update (Server.ProgressAmount (Just (i * 10)) (Just "Doing stuff"))
              liftIO $ Concurrent.threadDelay (1 * 1000000),
      requestHandler (LSP.SCustomMethod "notes-lsp/testing") $ \req responder -> do
        responder $ Right $ Aeson.Object mempty
    ]

notificationHandler ::
  forall (m :: LSP.Method 'LSP.FromClient 'LSP.Notification).
  LSP.SMethod m ->
  Server.Handler ServerM m ->
  Server.Handlers ServerM
notificationHandler = Server.notificationHandler

requestHandler ::
  forall (m :: LSP.Method 'LSP.FromClient 'LSP.Request).
  LSP.SMethod m ->
  Server.Handler ServerM' m ->
  Server.Handlers ServerM
requestHandler smethod fn = do
  Server.requestHandler smethod $ \msg k -> do
    st <- get
    env <- Server.getLspEnv
    let k' = State.toServer' . k
    let act = fn msg k'
    void $ liftIO $ Concurrent.forkIO $ State.runServer' st env act