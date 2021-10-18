module Markdown.Tree where

import qualified Commonmark
import Control.Exception (assert)
import qualified Data.Foldable as Foldable
import Data.Maybe (isNothing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Text.Pandoc.Builder as Pandoc

toList :: Seq a -> [a]
toList = Foldable.toList

singleton :: t -> Seq (t, Maybe a)
singleton = Seq.singleton . (,Nothing)

ilNull :: Inlines
ilNull = singleton IlNull

inLineHolder :: Inlines -> Blocks
inLineHolder = singleton . InlineHolder . toList

blNull :: Blocks
blNull = singleton BlNull

type Block = (Block', Maybe Commonmark.SourceRange)

data Block'
  = InlineHolder [Inline]
  | BlockHolder [Block]
  | BlNull
  deriving (Show, Eq)

-- instance Commonmark.IsBlock Inline Block where
type Blocks = Seq Block

instance Commonmark.Rangeable Blocks where
  ranged r bs = fmap (\(b, old) -> let !_ = assert $ isNothing old in (b, Just r)) bs

instance Commonmark.HasAttributes Blocks where
  addAttributes _ = id

instance Commonmark.IsBlock Inlines Blocks where
  paragraph = inLineHolder

  plain = inLineHolder

  thematicBreak = blNull

  blockQuote = id

  codeBlock _ _ = blNull

  heading _ = inLineHolder

  rawBlock _ _ = blNull

  referenceLinkDefinition _ _ = blNull

  list _ _ = singleton . BlockHolder . concatMap toList

type Inline = (Inline', Maybe Commonmark.SourceRange)

data Inline'
  = IlNull
  | Link [Inline] Text Text
  deriving (Show, Eq)

type Inlines = Seq Inline

instance Commonmark.Rangeable Inlines where
  ranged r bs = fmap (\(b, old) -> let !_ = assert $ isNothing old in (b, Just r)) bs

instance Commonmark.IsInline Inlines where
  lineBreak = ilNull

  softBreak = ilNull

  str = const ilNull

  entity = const ilNull

  escapedChar = const ilNull

  emph = id

  strong = id

  link dest title lDesc = singleton $ Link (toList lDesc) dest title

  image _ _ = id

  code _ = ilNull

  rawInline _ _ = ilNull

instance Commonmark.HasAttributes Inlines where
  addAttributes _ = id

instance Commonmark.Rangeable Inline where
  ranged r (il, old) = let !_ = assert $ isNothing old in (il, Just r)
