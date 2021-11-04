module Data.Rope.UtilsSpec (spec) where

import Data.Pos (Pos (..))
import qualified Data.Range as Range
import qualified Data.Rope.UTF16 as Rope
import qualified Data.Rope.Utils as Rope
import qualified Data.Span as Span
import MyPrelude
import Test.Hspec

spec :: Spec
spec = do
  let r = Range.new
  let s = Span.new'
  let p = Pos

  it "indexRange" $ do
    let rope = "hello"
    Rope.indexRange (r 0 2) rope `shouldBe` "he"
    Rope.indexRange (r 4 5) rope `shouldBe` "o"
    Rope.getRange (r 6 7) rope `shouldBe` Nothing
    Rope.getRange (r 3 1234) rope `shouldBe` Nothing
    Rope.getRange (r 3 4) rope `shouldBe` Just "l"

  it "indexSpan" $ do
    let !_ = traceShowId $ Rope.rowColumnCodeUnits (Rope.RowColumn 1234 1234) "adsfad"
    Rope.indexSpan (s 0 2 0 5) "hello" `shouldBe` "llo"
    Rope.getSpan (s 1 2 1 2) "hello" `shouldBe` Nothing
  return ()