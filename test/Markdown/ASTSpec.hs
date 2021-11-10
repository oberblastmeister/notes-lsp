module Markdown.ASTSpec (spec) where

import MyPrelude
import Test.Hspec
import qualified Data.Span as Span
import qualified Markdown.AST as AST
import Markdown.AST (AST(..))

spec :: Spec
spec = do
  it "smoke" $ do
    let ast = AST { meta = Nothing, elems = [(AST.Header, Span.new' 1 2 1 5)] }
    AST.containingElement (Span.new' 1 2 1 4) ast `shouldBe` Just (AST.Header, Span.new' 1 2 1 5)
    return @IO ()
  return ()