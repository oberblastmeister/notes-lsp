module Handlers
  ( allHandlers,
    Handlers,
    Handler,
  )
where

import qualified Config
import Control.Lens
import qualified Control.Lens as L
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Rope.UTF16 as Rope
import qualified Data.Span as Span
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Handlers.Completion
import Handlers.Utils
import qualified Language.LSP.Diagnostics as Diagnostics
import qualified Language.LSP.Server as Server
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified Language.LSP.VFS as VFS
import Logging
import qualified Markdown.AST as AST
import MyPrelude
import qualified Path
import qualified Proto
import ReactorMsg
import qualified Relude.Unsafe as Unsafe
import State (ServerState)
import qualified State
import UnliftIO.Async (async)
import UnliftIO.Concurrent as Concurrent
import qualified UnliftIO.Exception as Exception
import UnliftIO.STM (TQueue)
import qualified UnliftIO.STM as STM
import Utils (forMaybeM)
import qualified Utils

allHandlers :: TQueue ServerState -> TQueue ReactorAct -> Handlers
allHandlers stChan rChan =
  mconcat
    [ initialized stChan,
      workspaceDidChangeConfiguration,
      textDocumentDidOpen,
      textDocumentDidChange,
      textDocumentDidSave,
      textDocumentDefinition,
      textDocumentDidRename,
      textDocumentDidHover,
      textDocumentSymbol,
      textDocumentCodeAction,
      workspaceExecuteCommand,
      Handlers.Completion.handler
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
  debugM "handlers" "Processing Initialized notification"

  root <- Server.getRootPath <&> (>>= Path.parseAbsDir)
  folders <- Server.getWorkspaceFolders
  debugM "handlers" "Initializing state"
  void $ async $ initializeState stChan root folders

  -- We're initialized! Lets send a showMessageRequest now
  let params =
        LSP.ShowMessageRequestParams
          LSP.MtWarning
          "What's your favourite language extension?"
          (Just [LSP.MessageActionItem "Rank2Types", LSP.MessageActionItem "NPlusKPatterns"])

  void $
    Server.sendRequest LSP.SWindowShowMessageRequest params $ \case
      Left e -> errorM "handlers" $ "Got an error: " ++ show e
      Right _ -> do
        Server.sendNotification LSP.SWindowShowMessage $ LSP.ShowMessageParams LSP.MtInfo "Excellent choice"

        -- We can dynamically register a capability once the user accepts it
        Server.sendNotification LSP.SWindowShowMessage $ LSP.ShowMessageParams LSP.MtInfo "Turning on code lenses dynamically"

        let regOpts = LSP.CodeLensRegistrationOptions Nothing Nothing (Just False)

        void $
          Server.registerCapability LSP.STextDocumentCodeLens regOpts $ \_req responder -> do
            debugM "handlers" "Processing a textDocument/codeLens request"
            let cmd = LSP.Command "Say hello" "lsp-hello-command" Nothing
                rsp = LSP.List [LSP.CodeLens (LSP.mkRange 0 0 0 100) (Just cmd) Nothing]
            responder (Right rsp)

workspaceDidChangeConfiguration :: Handlers
workspaceDidChangeConfiguration = notificationHandler LSP.SWorkspaceDidChangeConfiguration $ \msg -> do
  cfg <- Server.getConfig
  debugM "configuration changed: " (show (msg, cfg))
  Server.sendNotification LSP.SWindowShowMessage $
    LSP.ShowMessageParams LSP.MtInfo $ "Wibble factor set to " <> T.pack (show $ cfg ^. #wibbleFactor)

textDocumentDidOpen :: Handlers
textDocumentDidOpen = notificationHandler LSP.STextDocumentDidOpen $ \msg -> do
  let doc = msg ^. LSP.params . LSP.textDocument . Proto.normalizedUri
  debugM "handlers" $ "Processing DidOpenTextDocument for: " ++ show doc
  vf <- Server.getVirtualFile doc
  case vf of
    Just (VFS.VirtualFile _ _ rope) -> do
      let path = doc ^. L.to (Unsafe.fromJust . LSP.uriToNormalizedFilePath)
      Utils.intoException $ State.changeNote path rope
    Nothing -> do
      debugM "handlers" $ "Didn't find anything in the VFS for: " ++ show doc

textDocumentDidChange :: Handlers
textDocumentDidChange = notificationHandler LSP.STextDocumentDidChange $ \msg -> do
  let doc =
        msg
          ^. LSP.params
            . LSP.textDocument
            . LSP.uri
            . to LSP.toNormalizedUri
  debugM "handlers" $ "Processing DidChangeTextDocument for: " ++ show doc
  vf <- Server.getVirtualFile doc
  case vf of
    Just (VFS.VirtualFile _ _ rope) -> do
      let path = doc ^. L.to (Unsafe.fromJust . LSP.uriToNormalizedFilePath)
      Utils.intoException $ State.changeNote path rope
    Nothing -> do
      debugM "handlers" $ "Didn't find anything in the VFS for: " ++ show doc

textDocumentDidSave :: Handlers
textDocumentDidSave = notificationHandler LSP.STextDocumentDidSave $ \msg -> do
  let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
      fileName = LSP.uriToFilePath doc
  debugM "handlers" $ "Processing DidSaveTextDocument  for: " ++ show fileName
  sendDiagnostics (LSP.toNormalizedUri doc) Nothing

textDocumentDidRename :: Handlers
textDocumentDidRename = requestHandler LSP.STextDocumentRename $ \req -> do
  debugM "handlers" "Processing a textDocument/rename request"
  let params = req ^. LSP.params
      LSP.Position l c = params ^. LSP.position
      newName = params ^. LSP.newName
  vdoc <- Server.getVersionedTextDoc (params ^. LSP.textDocument)
  -- Replace some text at the position with what the user entered
  let edit = LSP.InL $ LSP.TextEdit (LSP.mkRange l c l (c + T.length newName)) newName
      tde = LSP.TextDocumentEdit vdoc (LSP.List [edit])
      -- "documentChanges" field is preferred over "changes"
      rsp = LSP.WorkspaceEdit Nothing (Just (LSP.List [LSP.InL tde])) Nothing
  pure rsp

textDocumentDefinition :: Handlers
textDocumentDefinition = requestHandler LSP.STextDocumentDefinition $ \req -> do
  debugM "handlers" "Processing a textDocument/definition request"
  let params = req ^. LSP.params
      span = params ^. Proto.pos . to Span.empty
  note <- getNote params
  let astElem = Unsafe.fromJust $ AST.containingElement span (note ^. #ast)
  dest <- case astElem ^. L._1 of
    AST.LinkElement AST.WikiLink {dest} -> pure dest
    _ -> Exception.throwString "No link under the cursor"
  destNoteId <- L.use (#nameToNote . L.at dest) >>= Utils.fromJustMsg "Could not find destination of like"
  destNote <- gets $ State.getNote destNoteId
  let destNoteUri = destNote ^. #path . L.to Proto.normalizedFilePathToUri
  debugM "handlers" ("destNoteUri: " ++ show destNoteUri)
  pure $ LSP.InL $ Proto.topLocation destNoteUri

textDocumentDidHover :: Handlers
textDocumentDidHover = requestHandler LSP.STextDocumentHover $ \req -> do
  debugM "handlers" "Processing a textDocument/hover request"
  let LSP.HoverParams _doc pos _workDone = req ^. LSP.params
      LSP.Position _l _c' = pos
      rsp = LSP.Hover ms (Just range)
      ms = LSP.HoverContents $ LSP.markedUpContent "lsp-hello" "Your type info here!"
      range = LSP.Range pos pos
  pure $ Just rsp

textDocumentSymbol :: Handlers
textDocumentSymbol = requestHandler LSP.STextDocumentDocumentSymbol $ \req -> do
  debugM "handlers" "Processing a textDocument/documentSymbol request"
  let LSP.DocumentSymbolParams _ _ doc = req ^. LSP.params
      loc = LSP.Location (doc ^. LSP.uri) (LSP.Range (LSP.Position 0 0) (LSP.Position 0 0))
      sym = LSP.SymbolInformation "lsp-hello" LSP.SkFunction Nothing Nothing loc Nothing
      rsp = LSP.InR (LSP.List [sym])
  pure rsp

textDocumentCodeAction :: Handlers
textDocumentCodeAction = requestHandler LSP.STextDocumentCodeAction $ \req -> do
  debugM "handlers" "Processing a textDocument/codeAction request"
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
  pure rsp

workspaceExecuteCommand :: Handlers
workspaceExecuteCommand = requestHandler LSP.SWorkspaceExecuteCommand $ \req -> do
  debugM "handlers" "Processing a workspace/executeCommand request"
  let params = req ^. LSP.params
      margs = params ^. LSP.arguments

  debugM "handlers" $ "The arguments are: " ++ show margs
  -- responder (Right (Aeson.Object mempty)) -- respond to the request
  void $
    Server.withProgress "Executing some long running command" Server.Cancellable $ \update ->
      forM [0 .. 10] $ \(i :: Double) -> do
        update (Server.ProgressAmount (Just (i * 10)) (Just "Doing stuff"))
        Concurrent.threadDelay (1 * 1000000)
  pure $ Aeson.Object mempty

-- testing :: Handlers
-- testing = requestHandler (LSP.SCustomMethod "notes-lsp/testing") $ \req responder -> do
--   responder $ Right $ Aeson.Object mempty

initializeState :: (MonadUnliftIO m) => TQueue ServerState -> Maybe (Path Abs Dir) -> Maybe [LSP.WorkspaceFolder] -> m ()
initializeState rChan maybeRoot folders = do
  debugM "handlers" ("root " ++ show maybeRoot)
  debugM "handlers" ("folders " ++ show folders)
  case maybeRoot of
    Just root -> initializeState' rChan root
    Nothing -> pure ()

initializeState' :: (MonadUnliftIO m) => TQueue ServerState -> Path Abs Dir -> m ()
initializeState' stChan root = do
  files <- Utils.listDirFilesIgnore root
  let mdFiles = filter ((Just ".md" ==) . Path.fileExtension) files
  st <-
    execStateT
      ( do
          noteIds <- forMaybeM mdFiles $ \mdFile -> do
            contents <- liftIO $ TIO.readFile $ toFilePath mdFile
            let rope = Rope.fromText contents
            let nPath = LSP.toNormalizedFilePath $ toFilePath mdFile
            runExceptT
              (State.newNote nPath rope)
              >>= \case
                Left e -> pure Nothing
                Right noteId -> pure $ Just noteId
          debugM "handlers" ("Got noteIds" ++ show noteIds)
          forM_ noteIds State.updateNoteGraph
      )
      State.def
  atomically $ STM.writeTQueue stChan st