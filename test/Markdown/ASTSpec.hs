module Markdown.ASTSpec (spec) where

import qualified Control.Lens as L
import Control.Lens.Operators hiding ((#))
import Data.PositionBuilder (PositionExpr, (#))
import qualified Data.RangedText as RangedText
import qualified Data.Rope.UTF16 as Rope
import qualified Data.Rope.Utils as Rope
import qualified Markdown.AST as AST
import qualified Markdown.Parsing as Parsing
import MyPrelude
import Test.Hspec
import qualified Utils

checkContainingElem :: HasCallStack => PositionExpr -> Int -> Bool -> Expectation
checkContainingElem positionExpr pId has =
  if isJust res == has
    then pure ()
    else expectationFailure $ "Didn't get expected " ++ show has ++ "\nast: " ++ show ast ++ "\nspan: " ++ show span
  where
    rangedText = RangedText.build positionExpr & Utils.fromRightShow'
    text = rangedText ^. #text
    ast = Parsing.parseAST "<none>" text & Utils.fromRightShow'
    range = rangedText ^?! #marks . L.ix pId
    rope = Rope.fromText (rangedText ^. #text)
    span = Rope.rangeToSpan range rope
    res = AST.containingElement span ast

spec :: Spec
spec = do
  it "containing element" $ do
    {- ORMOLU_DISABLE -}
    let pe = 7#" "#6#"["#1#"["# 2#"a"#0#1#2#3#3#7#8#9#"dfas"#0#"d"#4#5#"f"#4#"]"#5#6#"]"#8#" "#9#" "
    {- ORMOLU_ENABLE -}
    checkContainingElem pe 0 True
    checkContainingElem pe 1 True
    checkContainingElem pe 2 True
    checkContainingElem pe 3 True
    checkContainingElem pe 4 True
    checkContainingElem pe 5 True
    checkContainingElem pe 6 True
    checkContainingElem pe 7 False
    checkContainingElem pe 8 True
    return @IO ()
  it "containing element on right edge" $ do
    {- ORMOLU_DISABLE -}
    let pe = "[["#0#1#2#"a]"#1#"]"#0#" "#2#" "
    {- ORMOLU_ENABLE -}
    checkContainingElem pe 1 True
    checkContainingElem pe 0 True
    checkContainingElem pe 2 False
    return @IO ()
  return ()