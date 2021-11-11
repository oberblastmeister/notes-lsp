module Data.MarkedText where

import qualified Control.Lens as L
import Control.Lens.Operators
import Data.PositionBuilder (PositionExpr)
import qualified Data.PositionBuilder as PositionBuilder
import Data.Traversable (for)
import MyPrelude

data MarkedText = MarkedText
  { text :: Text,
    marks :: IntMap Int
  }
  deriving (Show, Eq, Generic)

build :: PositionExpr -> Maybe MarkedText
build pe = do
  marks <- for (pb ^. #positions) $ \case
    [x] -> Just x
    _ -> Nothing
  Just $ MarkedText {text = pb ^. #text, marks}
  where
    pb = PositionBuilder.build pe