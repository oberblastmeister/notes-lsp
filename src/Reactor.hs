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
import qualified Data.IORef as IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Handlers
import qualified Language.LSP.Server as Server
import qualified Language.LSP.Types as Handlers
import qualified Language.LSP.Types as LSP
import MyPrelude
import Path (Dir, File, Rel, toFilePath, (</>))
import qualified Path
import qualified Path.IO as PIO
import State (ServerM, ServerState)
import qualified State
import qualified System.Exit as Exit
import System.Log.Logger (debugM)
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
      Server.executeCommandCommands = Just ["lsp-hello-command"],
      Server.completionTriggerCharacters = Just ['[', '#']
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
  { act :: ServerM (),
    env :: Config.LanguageContextEnv
  }

select :: MonadIO m => [STM a] -> m a
select = liftIO . STM.atomically . Monad.msum

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
        st <- ask
        State.runServer st env act
      ReactorMsgInitState newSt -> do
        st <- ask
        liftIO $ IORef.writeIORef st newSt

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: TQueue ReactorAct -> Server.Handlers ServerM
lspHandlers rin = Server.mapHandlers goReq goNot Handlers.allHandlers
  where
    send = liftIO . STM.atomically . STM.writeTQueue rin

    goReq :: forall (a :: LSP.Method 'LSP.FromClient 'LSP.Request). Handlers.Handler a -> Handlers.Handler a
    goReq f = \msg k -> do
      env <- Server.getLspEnv
      send ReactorAct {act = f msg k, env}

    goNot :: forall (a :: LSP.Method 'LSP.FromClient 'LSP.Notification). Handlers.Handler a -> Handlers.Handler a
    goNot f = \msg -> do
      env <- Server.getLspEnv
      send ReactorAct {act = f msg, env}