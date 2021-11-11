module Data.PositionBuilder
  ( PositionBuilder (..),
    PositionExpr,
    (#),
    build,
  )
where

import Control.Lens.Operators hiding ((#))
import qualified Data.ByteString as B
import qualified Data.IntMap as IntMap
import qualified Data.Text.Encoding as Text.Encoding
import MyPrelude
import qualified TextLen

data PositionBuilder = PositionBuilder
  { text :: Text,
    positions :: IntMap [Int]
  }
  deriving (Show, Eq, Generic)

instance Semigroup PositionBuilder where
  PositionBuilder {text, positions}
    <> PositionBuilder {text = text', positions = positions'} =
      PositionBuilder
        { text = text <> text',
          positions = IntMap.unionWith (++) positions positions'
        }

instance Monoid PositionBuilder where
  mempty = PositionBuilder {text = mempty, positions = mempty}
  mappend = append

textPositionBuilder :: Text -> PositionBuilder
textPositionBuilder t = PositionBuilder {text = t, positions = mempty}

idPositionBuilder :: Int -> Int -> PositionBuilder
idPositionBuilder i pos = PositionBuilder {text = mempty, positions = IntMap.singleton i [pos]}

data PositionExpr
  = Id !Int
  | Text Text
  | Combine PositionExpr PositionExpr
  deriving (Show, Eq, Generic)

instance IsString PositionExpr where
  fromString = Text . fromString

liftBin :: (Int -> Int -> Int) -> (PositionExpr -> PositionExpr -> PositionExpr)
liftBin f x y = Id $ f (x ^?! #_Id) (y ^?! #_Id)

liftUn :: (Int -> Int) -> (PositionExpr -> PositionExpr)
liftUn f x = Id $ f (x ^?! #_Id)

instance Num PositionExpr where
  fromInteger = Id . fromInteger
  (+) = liftBin (+)
  (*) = liftBin (*)
  abs = liftUn abs
  signum = liftUn signum
  negate = liftUn negate

(#) :: PositionExpr -> PositionExpr -> PositionExpr
(#) = Combine

infixr 9 #

append :: PositionBuilder -> PositionBuilder -> PositionBuilder
append PositionBuilder {text, positions} PositionBuilder {text = text', positions = positions'} =
  PositionBuilder
    { text = text <> text',
      positions = IntMap.unionWith (++) positions positions'
    }

build :: PositionExpr -> PositionBuilder
build = buildWith TextLen.len16

buildWith :: (Text -> Int) -> PositionExpr -> PositionBuilder
buildWith lenFn = go 0
  where
    go :: Int -> PositionExpr -> PositionBuilder
    go _offset (Text t) = textPositionBuilder t
    go offset (Id i) = idPositionBuilder i offset
    -- go _offset (Combine _p (Id _i)) = throwError "Cannot have an id at the end"
    -- go offset (Combine p (Id i)) = append <$> go offset p <*> pure (idPositionBuilder i offset)
    go offset (Combine (Id i) p) = append (idPositionBuilder i offset) (go offset p)
    -- left associated stuff is associated to the right
    go offset (Combine (Combine e1 e2) e3) = go offset (Combine e1 (Combine e2 e3))
    go offset (Combine (Text t) p) = append (textPositionBuilder t) (go offset' p)
      where
        !offset' = offset + fromIntegral (lenFn t)