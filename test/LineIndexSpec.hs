module LineIndexSpec (spec) where

import qualified LineIndex
import MyPrelude
import Test.Hspec

spec :: Spec
spec = do
  it "should create" $ do
    let text = "hello\nworld"
    let index = LineIndex.new text
    print index
    return @IO ()
  return ()