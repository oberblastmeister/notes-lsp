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
module Reactor (main, newServerDefinition) where

import qualified Config
import Control.Lens
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Handlers
import qualified Language.LSP.Server as Server
import qualified Language.LSP.Types as Handlers
import qualified Language.LSP.Types as LSP
import Logging
import MyPrelude
import qualified Path
import qualified Path.IO as PIO
import ReactorMsg
import State (ServerM, ServerState)
import qualified State
import qualified System.Exit as Exit
import qualified System.Log.Logger as Logger
import qualified Text.Show
import UnliftIO (async)
import qualified UnliftIO.Concurrent as Concurrent
import qualified UnliftIO.Exception as Exception
import UnliftIO.IORef as IORef
import UnliftIO.STM (TQueue)
import qualified UnliftIO.STM as STM

newtype ReactorException = ReactorException SomeException
  deriving (Typeable, Exception)

instance Text.Show.Show ReactorException where
  show (ReactorException e) = "Reactor exception: " ++ show e

main :: IO ()
main = do
  run >>= \case
    0 -> Exit.exitSuccess
    c -> Exit.exitWith . Exit.ExitFailure $ c

run :: IO Int
run =
  flip Exception.catches handlers $
    newServerDefinition >>= runServer
  where
    handlers =
      [ Exception.Handler ioExcept,
        Exception.Handler someExcept
      ]
    ioExcept (e :: Exception.IOException) = print e >> return 1
    someExcept (e :: Exception.SomeException) = print e >> return 1

newServerDefinition :: IO Config.ServerDefinition
newServerDefinition = do
  rChan <- STM.atomically STM.newTQueue
  stChan <- STM.atomically STM.newTQueue
  tid <- Concurrent.myThreadId
  defSt <- IORef.newIORef State.def

  pure $
    Server.ServerDefinition
      { defaultConfig = Config.def,
        onConfigurationChange = \_old v -> do
          case Aeson.fromJSON v of
            Aeson.Error e -> Left (T.pack e)
            Aeson.Success cfg -> Right cfg,
        doInitialize = \env _ -> do
          async $
            runReaderT (reactor stChan rChan) defSt
              -- if async exceptions come up we still want to propage them to the main thread
              -- cannot use Utils.async' here because the thread id is different
              `Exception.catchSyncOrAsync` (Exception.throwTo tid . ReactorException)
          pure $ Right env,
        staticHandlers = lspHandlers stChan rChan,
        interpretHandler = \env -> Server.Iso (State.runServer defSt env) liftIO,
        options = lspOptions
      }

runServer :: Config.ServerDefinition -> IO Int
runServer serverDefinition = do
  Exception.finally
    ( Exception.catchAny
        ( do
            setupLogger
            Server.runServer serverDefinition
        )
        ( \e -> do
            errorM "reactor" ("An exception occured: " ++ show e)
            Exception.throwIO e
        )
    )
    Logger.removeAllHandlers

setupLogger :: IO ()
setupLogger = do
  logDir <- PIO.getXdgDir PIO.XdgData $ Just [Path.reldir|notes-lsp|]
  PIO.ensureDir logDir
  let logFile = logDir </> [Path.relfile|notes-lsp.log|]
  unlessM (PIO.doesFileExist logFile) $ TIO.writeFile (toFilePath logFile) ""
  Server.setupLogger (Just $ toFilePath logFile) ["reactor", "handlers"] Logger.DEBUG

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

reactor :: (MonadReactor m) => TQueue ServerState -> TQueue ReactorAct -> m ()
reactor stChan rChan = do
  debugM "reactor" "Started the reactor"
  forever $ do
    msg <-
      (STM.atomically . Monad.msum)
        [ -- stm is not fair, and will always prefer the first action
          -- since getting the state is always more important than everything else
          -- we put it first in the list
          STM.readTQueue stChan <&> ReactorMsgInitState,
          STM.readTQueue rChan <&> ReactorMsgAct
        ]
    case msg of
      ReactorMsgAct ReactorAct {act, env} -> do
        stRef <- ask
        State.runServer stRef env act
      ReactorMsgInitState newSt -> do
        stRef <- ask
        debugM "reactor" ("Got st:\n" ++ show newSt)
        IORef.modifyIORef stRef (`State.combine` newSt)

-- | Check if we have a handler, and if we create a haskell-lsp handler to pass it as
-- input into the reactor
lspHandlers :: TQueue ServerState -> TQueue ReactorAct -> Server.Handlers ServerM
lspHandlers stChan rChan = Server.mapHandlers goReq goNot (Handlers.allHandlers stChan rChan)
  where
    send = STM.atomically . STM.writeTQueue rChan

    goReq :: forall (a :: LSP.Method 'LSP.FromClient 'LSP.Request). Handlers.Handler a -> Handlers.Handler a
    goReq f = \msg k -> do
      env <- Server.getLspEnv
      send ReactorAct {act = f msg k, env}

    goNot :: forall (a :: LSP.Method 'LSP.FromClient 'LSP.Notification). Handlers.Handler a -> Handlers.Handler a
    goNot f = \msg -> do
      env <- Server.getLspEnv
      send ReactorAct {act = f msg, env}