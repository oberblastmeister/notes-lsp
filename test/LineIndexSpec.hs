module LineIndexSpec (spec) where

import qualified Control.Lens as L
import Control.Lens.Operators
import qualified Data.Vector.Unboxed as VU
import LineIndex (LineIndex)
import qualified LineIndex
import MyPrelude
import Test.Hspec

checkConvert16 :: HasCallStack => Int -> Int -> Int -> LineIndex -> Expectation
checkConvert16 line col8 col16 index = do
  actualCol16 `shouldBe` col16
  LineIndex.utf16ToUtf8Col line actualCol16 index `shouldBe` col8
  where
    actualCol16 = LineIndex.utf8ToUtf16Col line col8 index

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
    let newLines =
          [ 0,
            1,
            7,
            15,
            21
          ]
    let index = LineIndex.new text
    index ^. #newLines `shouldBe` VU.fromList newLines
    -- print index

    index ^. #newLines . L.to VU.length `shouldBe` 5
    index ^. #nonAscii . L.at 1 `shouldBe` Nothing
    index ^?! #nonAscii . L.ix 2 . L.to VU.length `shouldBe` 1
    index ^?! #nonAscii . L.ix 3 . L.to VU.length `shouldBe` 1
    index ^. #nonAscii . L.at 4 `shouldBe` Nothing
    index ^. #nonAscii . L.at 5 `shouldBe` Nothing

    -- utf-8 to utf-16
    checkConvert16 1 3 3 index
    checkConvert16 2 5 3 index
    checkConvert16 2 6 4 index
    checkConvert16 3 4 2 index

    -- utf-16 to utf-8

    return @IO ()

  return ()