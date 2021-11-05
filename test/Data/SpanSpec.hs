module Data.SpanSpec (spec) where

import qualified Data.Span as Span
import MyPrelude
import Test.Hspec

spec :: Spec
spec = do
  let sp = Span.new'
  it "contains" $ do
    Span.contains (sp 1 3 1234 2) (sp 2 34 3 34) `shouldBe` True
    Span.contains (sp 1 3 1 3) (sp 1 3 1 3) `shouldBe` True
    Span.contains (sp 1 4 3 4) (sp 2 4 2 4) `shouldBe` True
    Span.contains (sp 1 4 1 9) (sp 1 5 1 6) `shouldBe` True
    Span.contains (sp 1 4 1 9) (sp 1 0 1 9) `shouldBe` False

  return ()