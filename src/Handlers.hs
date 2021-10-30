module Handlers
  ( allHandlers,
    Handlers,
    Handler,
  )
where

import qualified Config
import qualified Control.Concurrent as Concurrent
import Control.Concurrent.STM (TQueue)
import qualified Control.Concurrent.STM as STM
import Control.Exception (try)
import Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Rope.UTF16 as Rope
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Language.LSP.Diagnostics as Diagnostics
import qualified Language.LSP.Server as Server
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified Language.LSP.VFS as VFS
import MyPrelude
import qualified Path
import qualified Path.IO as PIO
import ReactorMsg
import qualified Relude.Unsafe as Unsafe
import State (ServerM, ServerM', ServerState)
import qualified State
import System.Log.Logger (debugM, errorM)
import Utils (forMaybeM)

type Handler a = Server.Handler ServerM a

type Handlers = Server.Handlers ServerM

allHandlers :: TQueue ServerState -> TQueue ReactorAct -> Handlers
allHandlers stChan rChan =
  mconcat
    [ initialized stChan,
      workspaceDidChangeConfiguration,
      textDocumentDidOpen,
      textDocumentDidChange,
      textDocumentDidSave,
      textDocumentDidRename,
      textDocumentDidHover,
      textDocumentSymbol,
      textDocumentCodeAction,
      workspaceExecuteCommand,
      textDocumentCompletion,
      testing
    ]

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

initialized :: TQueue ServerState -> Handlers
initialized stChan = notificationHandler LSP.SInitialized $ \_msg -> do
  liftIO $ debugM "handlers" "Processing Initialized notification"

  root <- Server.getRootPath <&> (>>= Path.parseAbsDir)
  folders <- Server.getWorkspaceFolders
  liftIO $ debugM "handlers" "Initializing state"
  liftIO $ Concurrent.forkIO $ initializeState stChan root folders

  -- We're initialized! Lets send a showMessageRequest now
  let params =
        LSP.ShowMessageRequestParams
          LSP.MtWarning
          "What's your favourite language extension?"
          (Just [LSP.MessageActionItem "Rank2Types", LSP.MessageActionItem "NPlusKPatterns"])

  void $
    Server.sendRequest LSP.SWindowShowMessageRequest params $ \case
      Left e -> liftIO $ errorM "handlers" $ "Got an error: " ++ show e
      Right _ -> do
        Server.sendNotification LSP.SWindowShowMessage $ LSP.ShowMessageParams LSP.MtInfo "Excellent choice"

        -- We can dynamically register a capability once the user accepts it
        Server.sendNotification LSP.SWindowShowMessage $ LSP.ShowMessageParams LSP.MtInfo "Turning on code lenses dynamically"

        let regOpts = LSP.CodeLensRegistrationOptions Nothing Nothing (Just False)

        void $
          Server.registerCapability LSP.STextDocumentCodeLens regOpts $ \_req responder -> do
            liftIO $ debugM "handlers" "Processing a textDocument/codeLens request"
            let cmd = LSP.Command "Say hello" "lsp-hello-command" Nothing
                rsp = LSP.List [LSP.CodeLens (LSP.mkRange 0 0 0 100) (Just cmd) Nothing]
            responder (Right rsp)

workspaceDidChangeConfiguration :: Handlers
workspaceDidChangeConfiguration = notificationHandler LSP.SWorkspaceDidChangeConfiguration $ \msg -> do
  cfg <- Server.getConfig
  liftIO $ debugM "configuration changed: " (show (msg, cfg))
  Server.sendNotification LSP.SWindowShowMessage $
    LSP.ShowMessageParams LSP.MtInfo $ "Wibble factor set to " <> T.pack (show $ cfg ^. #wibbleFactor)

textDocumentDidOpen :: Handlers
textDocumentDidOpen = notificationHandler LSP.STextDocumentDidOpen $ \msg -> do
  let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
      fileName = LSP.uriToFilePath doc
  liftIO $ debugM "handlers" $ "Processing DidOpenTextDocument for: " ++ show fileName
  sendDiagnostics (LSP.toNormalizedUri doc) (Just 0)

textDocumentDidChange :: Handlers
textDocumentDidChange = notificationHandler LSP.STextDocumentDidChange $ \msg -> do
  let doc =
        msg
          ^. LSP.params
            . LSP.textDocument
            . LSP.uri
            . to LSP.toNormalizedUri
  liftIO $ debugM "handlers" $ "Processing DidChangeTextDocument for: " ++ show doc
  mdoc <- Server.getVirtualFile doc
  case mdoc of
    Just doc@(VFS.VirtualFile _version str _) -> do
      liftIO $ do
        debugM "handlers" $ "Found the virtual file: " ++ show str
        debugM "handlers" $ "Virtual file text: " ++ show (VFS.virtualFileText doc)
    Nothing -> do
      liftIO $ debugM "handlers" $ "Didn't find anything in the VFS for: " ++ show doc

textDocumentDidSave :: Handlers
textDocumentDidSave = notificationHandler LSP.STextDocumentDidSave $ \msg -> do
  let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
      fileName = LSP.uriToFilePath doc
  liftIO $ debugM "handlers" $ "Processing DidSaveTextDocument  for: " ++ show fileName
  sendDiagnostics (LSP.toNormalizedUri doc) Nothing

textDocumentDidRename :: Handlers
textDocumentDidRename = requestHandler LSP.STextDocumentRename $ \req responder -> do
  liftIO $ debugM "handlers" "Processing a textDocument/rename request"
  let params = req ^. LSP.params
      LSP.Position l c = params ^. LSP.position
      newName = params ^. LSP.newName
  vdoc <- Server.getVersionedTextDoc (params ^. LSP.textDocument)
  -- Replace some text at the position with what the user entered
  let edit = LSP.InL $ LSP.TextEdit (LSP.mkRange l c l (c + T.length newName)) newName
      tde = LSP.TextDocumentEdit vdoc (LSP.List [edit])
      -- "documentChanges" field is preferred over "changes"
      rsp = LSP.WorkspaceEdit Nothing (Just (LSP.List [LSP.InL tde])) Nothing
  responder (Right rsp)

textDocumentDidHover :: Handlers
textDocumentDidHover = requestHandler LSP.STextDocumentHover $ \req responder -> do
  liftIO $ debugM "handlers" "Processing a textDocument/hover request"
  let LSP.HoverParams _doc pos _workDone = req ^. LSP.params
      LSP.Position _l _c' = pos
      rsp = LSP.Hover ms (Just range)
      ms = LSP.HoverContents $ LSP.markedUpContent "lsp-hello" "Your type info here!"
      range = LSP.Range pos pos
  responder (Right $ Just rsp)

textDocumentSymbol :: Handlers
textDocumentSymbol = requestHandler LSP.STextDocumentDocumentSymbol $ \req responder -> do
  liftIO $ debugM "handlers" "Processing a textDocument/documentSymbol request"
  let LSP.DocumentSymbolParams _ _ doc = req ^. LSP.params
      loc = LSP.Location (doc ^. LSP.uri) (LSP.Range (LSP.Position 0 0) (LSP.Position 0 0))
      sym = LSP.SymbolInformation "lsp-hello" LSP.SkFunction Nothing Nothing loc Nothing
      rsp = LSP.InR (LSP.List [sym])
  responder (Right rsp)

textDocumentCodeAction :: Handlers
textDocumentCodeAction = requestHandler LSP.STextDocumentCodeAction $ \req responder -> do
  liftIO $ debugM "handlers" "Processing a textDocument/codeAction request"
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
  responder (Right rsp)

workspaceExecuteCommand :: Handlers
workspaceExecuteCommand = requestHandler LSP.SWorkspaceExecuteCommand $ \req responder -> do
  liftIO $ debugM "handlers" "Processing a workspace/executeCommand request"
  let params = req ^. LSP.params
      margs = params ^. LSP.arguments

  liftIO $ debugM "handlers" $ "The arguments are: " ++ show margs
  responder (Right (Aeson.Object mempty)) -- respond to the request
  void $
    Server.withProgress "Executing some long running command" Server.Cancellable $ \update ->
      forM [0 .. 10] $ \(i :: Double) -> do
        update (Server.ProgressAmount (Just (i * 10)) (Just "Doing stuff"))
        liftIO $ Concurrent.threadDelay (1 * 1000000)

textDocumentCompletion :: Handlers
textDocumentCompletion = requestHandler LSP.STextDocumentCompletion $ \req responder -> do
  let params = req ^. LSP.params
      uri = params ^. LSP.textDocument . LSP.uri . to LSP.toNormalizedUri
  -- prefix <- VFS.getCompletionPrefix Position VirtualFile
  vf <- Server.getVirtualFile uri <&> Unsafe.fromJust

  -- liftIO $ debugM "handlers" $ show params
  let items =
        LSP.List $
          fmap
            (`completionItem` LSP.CiFile)
            [ "first",
              "second"
            ]
  responder $ Right $ LSP.InL items

-- shouldComplete :: Rope -> Bool
-- shouldCompletekj

completionItem :: Text -> LSP.CompletionItemKind -> LSP.CompletionItem
completionItem _label _kind =
  LSP.CompletionItem
    { _label,
      _kind = Just _kind,
      _tags = Nothing,
      _detail = Nothing,
      _documentation = Nothing,
      _deprecated = Nothing,
      _preselect = Nothing,
      _sortText = Nothing,
      _filterText = Nothing,
      _insertText = Nothing,
      _insertTextFormat = Nothing,
      _insertTextMode = Nothing,
      _textEdit = Nothing,
      _additionalTextEdits = Nothing,
      _commitCharacters = Nothing,
      _command = Nothing,
      _xdata = Nothing
    }

testing :: Handlers
testing = requestHandler (LSP.SCustomMethod "notes-lsp/testing") $ \req responder -> do
  responder $ Right $ Aeson.Object mempty

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

initializeState :: TQueue ServerState -> Maybe (Path Abs Dir) -> Maybe [LSP.WorkspaceFolder] -> IO ()
initializeState rChan maybeRoot folders = do
  liftIO $ debugM "handlers" ("root " ++ show maybeRoot)
  liftIO $ debugM "handlers" ("folders " ++ show folders)
  case maybeRoot of
    Just root -> initializeState' rChan root
    Nothing -> pure ()

initializeState' :: TQueue ServerState -> Path Abs Dir -> IO ()
initializeState' stChan root = do
  (_, files) <- PIO.listDirRecur root
  mdFiles <-
    filterM
      ( \file ->
          try @SomeException (Path.fileExtension file)
            <&> rightToMaybe
            <&> fmap (== ".md")
            <&> fromMaybe False
      )
      files
  st <-
    execStateT
      ( do
          noteIds <- forMaybeM mdFiles $ \mdFile -> do
            contents <- liftIO $ TIO.readFile $ toFilePath mdFile
            let rope = Rope.fromText contents
            let nPath = LSP.toNormalizedFilePath $ toFilePath $ mdFile
            runExceptT
              (State.addPath nPath contents rope)
              >>= \case
                Left e -> pure Nothing
                Right noteId -> pure $ Just noteId
          liftIO $ debugM "handlers" ("Got noteIds" ++ show noteIds)
          forM_ noteIds $ \noteId -> do
            State.updateGraph noteId
      )
      State.def
  atomically $ STM.writeTQueue stChan st