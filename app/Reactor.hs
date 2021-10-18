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

import Control.Concurrent
import Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Language.LSP.Diagnostics as Diagnostics
import Language.LSP.Server (LspM)
import qualified Language.LSP.Server as Server
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified Language.LSP.VFS as VFS
import Lens.Micro.Platform
import System.Exit
import System.Log.Logger

main :: IO ()
main = do
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

-- ---------------------------------------------------------------------

data Config = Config {fooTheBar :: Bool, wibbleFactor :: Int}
  deriving (Generic, Show)

instance Aeson.ToJSON Config

instance Aeson.FromJSON Config

run :: IO Int
run = flip E.catches handlers $ do
  rin <- atomically $ newTChan @ReactorInput

  let serverDefinition =
        Server.ServerDefinition
          { defaultConfig = Config {fooTheBar = False, wibbleFactor = 0},
            onConfigurationChange = \_old v -> do
              case Aeson.fromJSON v of
                Aeson.Error e -> Left (T.pack e)
                Aeson.Success cfg -> Right cfg,
            doInitialize = \env _ -> forkIO (reactor rin) >> pure (Right env),
            staticHandlers = lspHandlers rin,
            interpretHandler = \env -> Server.Iso (Server.runLspT env) liftIO,
            options = lspOptions
          }

  flip E.finally finalProc $ do
    Server.setupLogger Nothing ["reactor"] DEBUG
    Server.runServer serverDefinition
  where
    handlers =
      [ E.Handler ioExcept,
        E.Handler someExcept
      ]
    finalProc = removeAllHandlers
    ioExcept (e :: E.IOException) = print e >> return 1
    someExcept (e :: E.SomeException) = print e >> return 1

-- ---------------------------------------------------------------------

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

-- ---------------------------------------------------------------------

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

newtype ReactorInput
  = ReactorAction (IO ())

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: LSP.NormalizedUri -> Maybe Int -> LspM Config ()
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

-- ---------------------------------------------------------------------

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: TChan ReactorInput -> IO ()
reactor inp = do
  debugM "reactor" "Started the reactor"
  forever $ do
    ReactorAction act <- atomically $ readTChan inp
    act

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: TChan ReactorInput -> Server.Handlers (LspM Config)
lspHandlers rin = Server.mapHandlers goReq goNot handle
  where
    goReq :: forall (a :: LSP.Method 'LSP.FromClient 'LSP.Request). Server.Handler (LspM Config) a -> Server.Handler (LspM Config) a
    goReq f = \msg k -> do
      env <- Server.getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (Server.runLspT env $ f msg k)

    goNot :: forall (a :: LSP.Method 'LSP.FromClient 'LSP.Notification). Server.Handler (LspM Config) a -> Server.Handler (LspM Config) a
    goNot f = \msg -> do
      env <- Server.getLspEnv
      liftIO $ atomically $ writeTChan rin $ ReactorAction (Server.runLspT env $ f msg)

-- | Where the actual logic resides for handling requests and notifications.
handle :: Server.Handlers (LspM Config)
handle =
  mconcat
    [ Server.notificationHandler LSP.SInitialized $ \_msg -> do
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
              Server.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtInfo "Excellent choice")

              -- We can dynamically register a capability once the user accepts it
              Server.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtInfo "Turning on code lenses dynamically")

              let regOpts = LSP.CodeLensRegistrationOptions Nothing Nothing (Just False)

              void $
                Server.registerCapability LSP.STextDocumentCodeLens regOpts $ \_req responder -> do
                  liftIO $ debugM "reactor.handle" "Processing a textDocument/codeLens request"
                  let cmd = LSP.Command "Say hello" "lsp-hello-command" Nothing
                      rsp = LSP.List [LSP.CodeLens (LSP.mkRange 0 0 0 100) (Just cmd) Nothing]
                  responder (Right rsp),
      Server.notificationHandler LSP.STextDocumentDidOpen $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            fileName = LSP.uriToFilePath doc
        liftIO $ debugM "reactor.handle" $ "Processing DidOpenTextDocument for: " ++ show fileName
        sendDiagnostics (LSP.toNormalizedUri doc) (Just 0),
      Server.notificationHandler LSP.SWorkspaceDidChangeConfiguration $ \msg -> do
        cfg <- Server.getConfig
        liftIO $ debugM "configuration changed: " (show (msg, cfg))
        Server.sendNotification LSP.SWindowShowMessage $
          LSP.ShowMessageParams LSP.MtInfo $ "Wibble factor set to " <> T.pack (show (wibbleFactor cfg)),
      Server.notificationHandler LSP.STextDocumentDidChange $ \msg -> do
        let doc =
              msg
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to LSP.toNormalizedUri
        liftIO $ debugM "reactor.handle" $ "Processing DidChangeTextDocument for: " ++ show doc
        mdoc <- Server.getVirtualFile doc
        case mdoc of
          Just (VFS.VirtualFile _version str _) -> do
            liftIO $ debugM "reactor.handle" $ "Found the virtual file: " ++ show str
          Nothing -> do
            liftIO $ debugM "reactor.handle" $ "Didn't find anything in the VFS for: " ++ show doc,
      Server.notificationHandler LSP.STextDocumentDidSave $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            fileName = LSP.uriToFilePath doc
        liftIO $ debugM "reactor.handle" $ "Processing DidSaveTextDocument  for: " ++ show fileName
        sendDiagnostics (LSP.toNormalizedUri doc) Nothing,
      Server.requestHandler LSP.STextDocumentRename $ \req responder -> do
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
      Server.requestHandler LSP.STextDocumentHover $ \req responder -> do
        liftIO $ debugM "reactor.handle" "Processing a textDocument/hover request"
        let LSP.HoverParams _doc pos _workDone = req ^. LSP.params
            LSP.Position _l _c' = pos
            rsp = LSP.Hover ms (Just range)
            ms = LSP.HoverContents $ LSP.markedUpContent "lsp-hello" "Your type info here!"
            range = LSP.Range pos pos
        responder (Right $ Just rsp),
      Server.requestHandler LSP.STextDocumentDocumentSymbol $ \req responder -> do
        liftIO $ debugM "reactor.handle" "Processing a textDocument/documentSymbol request"
        let LSP.DocumentSymbolParams _ _ doc = req ^. LSP.params
            loc = LSP.Location (doc ^. LSP.uri) (LSP.Range (LSP.Position 0 0) (LSP.Position 0 0))
            sym = LSP.SymbolInformation "lsp-hello" LSP.SkFunction Nothing Nothing loc Nothing
            rsp = LSP.InR (LSP.List [sym])
        responder (Right rsp),
      Server.requestHandler LSP.STextDocumentCodeAction $ \req responder -> do
        liftIO $ debugM "reactor.handle" "Processing a textDocument/codeAction request"
        let params = req ^. LSP.params
            doc = params ^. LSP.textDocument
            (LSP.List diags) = params ^. LSP.context . LSP.diagnostics
            -- makeCommand only generates commands for diagnostics whose source is us
            makeCommand (LSP.Diagnostic (LSP.Range start _) _s _c (Just "lsp-hello") _m _t _l) = [LSP.Command title cmd cmdparams]
              where
                title = "Apply LSP hello command:" <> head (T.lines _m)
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
      Server.requestHandler LSP.SWorkspaceExecuteCommand $ \req responder -> do
        liftIO $ debugM "reactor.handle" "Processing a workspace/executeCommand request"
        let params = req ^. LSP.params
            margs = params ^. LSP.arguments

        liftIO $ debugM "reactor.handle" $ "The arguments are: " ++ show margs
        responder (Right (Aeson.Object mempty)) -- respond to the request
        void $
          Server.withProgress "Executing some long running command" Server.Cancellable $ \update ->
            forM [(0 :: Double) .. 10] $ \i -> do
              update (Server.ProgressAmount (Just (i * 10)) (Just "Doing stuff"))
              liftIO $ threadDelay (1 * 1000000)
    ]