module ServerSpec (spec) where

import Language.LSP.Test (Session)
import qualified Language.LSP.Test as LSP.Test
import qualified Language.LSP.Types as LSP
import MyPrelude
import Test.Hspec

lspTest :: String -> Session () -> Spec
lspTest name session =
  it name $
    LSP.Test.runSession "notes-lsp" LSP.Test.fullCaps "test_data/lsp" session

emptyRange :: Int -> Int -> LSP.Range
emptyRange l c = LSP.Range (LSP.Position l c) (LSP.Position l c)

insertEdit :: Int -> Int -> Text -> LSP.TextEdit
insertEdit l c = LSP.TextEdit (emptyRange l c)

spec :: Spec
spec = do
  lspTest "smoke" $ do
    doc <- LSP.Test.createDoc "test.md" "markdown" "hello world!"
    -- LSP.Test.applyEdit doc (insertEdit 0 0 "[")
    completions <- LSP.Test.getCompletions doc (LSP.Position 0 0)
    print completions
    return ()

  return ()