module LineIndexSpec (spec) where

import Control.Lens.Operators
import qualified Data.Vector.Unboxed as VU
import qualified LineIndex
import MyPrelude
import Test.Hspec

spec :: Spec
spec = do
  it "creation" $ do
    let text = "hello\nworld"
    let table =
          [ (0, 0, 0),
            (1, 0, 1),
            (5, 0, 5),
            (6, 1, 0),
            (9, 1, 3)
          ]
    let newLines =
          [ 0,
            6
          ]
    let index = LineIndex.new text
    index ^. #newLines `shouldBe` VU.fromList newLines
    forM_ table $ \(offset, line, col) -> do
      LineIndex.lineCol offset index `shouldBe` LineIndex.LineCol {line, col}
    return @IO ()

  it "get correct char len" $ do
    LineIndex.charLen 'メ' `shouldBe` 3
    LineIndex.charLenUtf16 'メ' `shouldBe` 1
    LineIndex.charLen '😀' `shouldBe` 4
    LineIndex.charLenUtf16 '😀' `shouldBe` 2

  it "convert properly" $ do
    let text = "\nasdfa\nメasfa\n😀f\n"
    let newLines = [
              0,
              1,
              7,
              15,
              21
            ]
    let index = LineIndex.new text
    index ^. #newLines `shouldBe` VU.fromList newLines
    print index
    return @IO ()

  return ()