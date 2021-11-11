module Language.LSP.Test.Utils
  ( runSession,
    lit,
    emptyRange,
    insertEdit,
    openDoc,
    getDefinitions,
    module X,
  )
where

import Config (Config (..))
import qualified Control.Lens as L
import Control.Lens.Operators
import qualified Data.Aeson as Aeson
import Data.Pos (Pos)
import qualified Data.Pos as Pos
import qualified Language.LSP.Server as Server
import Language.LSP.Test (Session)
import Language.LSP.Test as X (getDocUri)
import qualified Language.LSP.Test as LSP.Test
import qualified Language.LSP.Types as LSP
import MyPrelude
import Reactor (newServerDefinition)
import qualified System.Exit as Exit
import Test.Hspec
import UnliftIO (withAsync)
import qualified UnliftIO.Async as Async
import qualified UnliftIO.Exception as Exception
import qualified UnliftIO.Process as Process

defServerConfig :: Config
defServerConfig = def {waitInitializeState = True}

defaultConfig :: LSP.Test.SessionConfig
defaultConfig =
  LSP.Test.defaultConfig
    { LSP.Test.lspConfig = Just $ Aeson.toJSON defServerConfig
    }

runSession :: FilePath -> Session () -> IO ()
runSession path session = do
  (hinRead, hinWrite) <- Process.createPipe
  (houtRead, houtWrite) <- Process.createPipe

  serverDefinition <- newServerDefinition

  -- void $
  --   Async.concurrently
  --     ( do
  --         Left (Just Exit.ExitSuccess) <- first Exception.fromException <$> Exception.try (Server.runServerWithHandles hinRead houtWrite serverDefinition)
  --         pure ()
  --     )
  --     (LSP.Test.runSessionWithHandles hinWrite houtRead defaultConfig LSP.Test.fullCaps path session)

  withAsync (Server.runServerWithHandles hinRead houtWrite serverDefinition) $ \a1 -> do
    withAsync (LSP.Test.runSessionWithHandles hinWrite houtRead defaultConfig LSP.Test.fullCaps path session) $ \a2 -> do
      Async.wait a2
      Left (Just Exit.ExitSuccess) <- Async.waitCatch a1 <&> first Exception.fromException
      pure ()

lit :: FilePath -> String -> Session () -> Spec
lit path name session =
  it name $ do
    runSession path session

emptyRange :: Int -> Int -> LSP.Range
emptyRange l c = LSP.Range (LSP.Position l c) (LSP.Position l c)

insertEdit :: Int -> Int -> Text -> LSP.TextEdit
insertEdit l c = LSP.TextEdit (emptyRange l c)

getDefinitions :: LSP.TextDocumentIdentifier -> Pos -> Session ([LSP.Location] LSP.|? [LSP.LocationLink])
getDefinitions ident pos = LSP.Test.getDefinitions ident (pos ^. L.from Pos.position)

openDoc :: FilePath -> LSP.Test.Session LSP.TextDocumentIdentifier
openDoc path = LSP.Test.openDoc path "markdown"