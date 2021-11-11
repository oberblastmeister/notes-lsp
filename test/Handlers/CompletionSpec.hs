module Handlers.CompletionSpec (spec) where

import Test.Hspec
import MyPrelude
import Language.LSP.Test.Utils
import qualified Language.LSP.Test as LSP.Test
import qualified Language.LSP.Types as LSP

spec :: Spec
spec = do
  lit "test_data/lsp/empty" "smoke" $ do
    doc <- LSP.Test.createDoc "test.md" "markdown" "hello world!"
    LSP.Test.applyEdit doc (insertEdit 0 0 "[[")
    completions <- LSP.Test.getCompletions doc (LSP.Position 0 0)
    putStrLn $ "Completions: " ++ show completions
    return ()

  return ()