module Data.RangedText where

import qualified Control.Lens as L
import Control.Lens.Operators
import Control.Monad.Except
import Data.PositionBuilder (PositionExpr)
import qualified Data.PositionBuilder as PositionBuilder
import Data.Range (Range)
import qualified Data.Range as Range
import Data.Traversable (for)
import MyPrelude

data RangedText = RangedText
  { text :: Text,
    marks :: IntMap Range
  }
  deriving (Show, Eq, Generic)

build :: MonadError Text m => PositionExpr -> m RangedText
build pe = do
  marks <- for (pb ^. #positions) $ \case
    [x, x'] -> pure $ Range.new x x'
    _ -> throwError "Must only have two positions to form a range"
  pure $ RangedText {text = pb ^. #text, marks}
  where
    pb = PositionBuilder.build pe