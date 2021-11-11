module Handlers.DefinitionSpec (spec) where

import qualified Control.Exception as Exception'
import qualified Control.Lens as L
import Control.Lens.Operators
import Data.Pos (Pos (..))
import qualified Data.Span as Span
import qualified Language.LSP.Test as LSP.Test
import Language.LSP.Test.Utils hiding (lit)
import qualified Language.LSP.Test.Utils as LSP.Test.Utils
import qualified Language.LSP.Types as LSP
import MyPrelude
import qualified System.FilePath as FilePath
import Test.Hspec.Lifted

lit :: FilePath -> String -> LSP.Test.Session () -> Spec
lit path = LSP.Test.Utils.lit ("test_data/lsp/definition" FilePath.</> path)

simpleLocation :: LSP.Uri -> LSP.Location
simpleLocation _uri = LSP.Location {_uri, _range = Span.empty (Pos 0 0) ^. L.from Span.range}

simpleDefinitionRes :: LSP.Uri -> [LSP.Location] LSP.|? [LSP.LocationLink]
simpleDefinitionRes uri = LSP.InL [simpleLocation uri]

checkDefinitions :: FilePath -> Pos -> FilePath -> LSP.Test.Session ()
checkDefinitions startPath pos endPath = do
  doc <- openDoc startPath
  defs <- getDefinitions doc pos
  toUri <- getDocUri endPath
  defs `shouldBe` simpleDefinitionRes toUri

-- checkDefinitionsFail :: FilePath -> Pos -> LSP.Test.Session ()
-- checkDefinitionsFail startPath pos = do
--   doc <- openDoc startPath
--   res <- Exception'.try $ getDefinitions doc pos
--   isLeft (res) `shouldBe` True

spec :: Spec
spec = do
  lit "simple" "simple" $ do
    checkDefinitions "first.md" Pos {line = 0, col = 0} "second.md"
    checkDefinitions "first.md" Pos {line = 1, col = 2} "third.md"
    checkDefinitions "second.md" Pos {line = 0, col = 0} "first.md"
    checkDefinitions "third.md" Pos {line = 0, col = 0} "second.md"

  lit "math" "math" $ do
    checkDefinitions "derivative_rules.md" Pos {line = 6, col = 2} "derivative/simple_rules.md"

  return ()