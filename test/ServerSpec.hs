module ServerSpec (spec) where

import qualified Language.LSP.Server as Server
import Language.LSP.Test (Session)
import qualified Language.LSP.Test as LSP.Test
import qualified Language.LSP.Types as LSP
import MyPrelude
import Reactor (newServerDefinition)
import Test.Hspec
import qualified Text.Show
import UnliftIO (async, withAsync)
import qualified UnliftIO.Exception as Exception
import qualified UnliftIO.Process as Process

runSession :: Session () -> IO ()
runSession session = do
  (hinRead, hinWrite) <- Process.createPipe
  (houtRead, houtWrite) <- Process.createPipe

  serverDefinition <- newServerDefinition

  withAsync (Server.runServerWithHandles hinRead houtWrite serverDefinition) $ \_ -> do
    LSP.Test.runSessionWithHandles hinWrite houtRead LSP.Test.defaultConfig LSP.Test.fullCaps "test_data/lsp" session

lit :: String -> Session () -> Spec
lit name session =
  it name $ do
    runSession session

emptyRange :: Int -> Int -> LSP.Range
emptyRange l c = LSP.Range (LSP.Position l c) (LSP.Position l c)

insertEdit :: Int -> Int -> Text -> LSP.TextEdit
insertEdit l c = LSP.TextEdit (emptyRange l c)

spec :: Spec
spec = do
  lit "smoke" $ do
    doc <- LSP.Test.createDoc "test.md" "markdown" "hello world!"
    LSP.Test.applyEdit doc (insertEdit 0 0 "[[")
    -- LSP.Test.getCompletions doc (LSP.Position 0 0)
    -- print completions
    return ()

  return ()