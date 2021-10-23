module Markdown.CST where

import qualified Commonmark
import qualified Commonmark.Extensions as Extensions
import Control.Lens
import qualified Data.Sequence as Seq
import Markdown.Connection (Connection)
import qualified Markdown.Links as Links
import MyPrelude

singleton :: t -> Seq (t, Maybe a)
singleton = Seq.singleton . (,Nothing)

ilNull :: Inlines
ilNull = singleton IlNull

ilInlineHolder :: Inlines -> Inlines
ilInlineHolder = singleton . IlInlineHolder . toList

blBlHolder :: Blocks -> Blocks
blBlHolder = singleton . BlBlockHolder . toList

inLineHolder :: Inlines -> Blocks
inLineHolder = singleton . BlInlineHolder . toList

blNull :: Blocks
blNull = singleton BlNull

note :: Blocks -> Inlines
note = singleton . Note . toList

addRange :: (Show a, HasCallStack) => (a, Maybe Commonmark.SourceRange) -> Commonmark.SourceRange -> (a, Maybe Commonmark.SourceRange)
addRange old pos =
  let !_ =
        if isJust (old ^. _2)
          then error $ "setting a span range for: " <> show (old ^. _1)
          else ()
   in old & _2 ?~ pos

type Block = (Block', Maybe Commonmark.SourceRange)

data Block'
  = BlInlineHolder [Inline]
  | BlBlockHolder [Block]
  | Heading Int [Inline]
  | -- | DefinitionList [([Inline], [Block])]
    BlNull
  deriving (Show, Eq, Generic)

-- instance Commonmark.IsBlock Inline Block where
type Blocks = Seq Block

instance Plated Block where
  plate f (b', r) = (,r) <$> plateBlock' f b'

plateBlock' :: Applicative f => (Block -> f Block) -> Block' -> f Block'
plateBlock' f = \case
  BlBlockHolder bs -> BlBlockHolder <$> traverse f bs
  other -> pure other

instance Commonmark.Rangeable Blocks where
  ranged r bs = bs & each %~ (`addRange` r)

instance Commonmark.HasAttributes Blocks where
  addAttributes _ = blBlHolder

instance Commonmark.IsBlock Inlines Blocks where
  paragraph = inLineHolder

  plain = inLineHolder

  thematicBreak = blNull

  blockQuote = blBlHolder

  codeBlock _ _ = blNull

  heading i il = singleton $ Heading i $ toList il

  rawBlock _ _ = blNull

  referenceLinkDefinition _ _ = blNull

  list _ _ bls = bls ^.. each . each ^. to BlBlockHolder . to singleton

instance Extensions.HasDiv Blocks where
  div_ = blBlHolder

instance Extensions.HasPipeTable Inlines Blocks where
  pipeTable _ ils ilss = singleton $ BlInlineHolder $ concatMap toList ils ++ concatMap (concatMap toList) ilss

instance Extensions.HasTaskList Inlines Blocks where
  taskList _ _ stuff = stuff ^.. each . _2 . each ^. to (singleton . BlBlockHolder)

instance Extensions.HasFootnote Inlines Blocks where
  footnote _ _ = blBlHolder

  footnoteList bls = bls ^.. each . each ^. to (singleton . BlBlockHolder)

  footnoteRef _ _ = note

-- instance Extensions.HasDefinitionList Inlines Blocks where
-- definitionList _ = _

type Inline = (Inline', Maybe Commonmark.SourceRange)

data Inline'
  = IlNull
  | IlInlineHolder [Inline]
  | IlWikiLink WikiLink
  | Note [Block]
  deriving (Show, Eq, Generic)

data WikiLink = WikiLink
  { conn :: Connection,
    url :: Text,
    name :: Maybe Text
  }
  deriving (Show, Eq, Generic)

type Inlines = Seq Inline

instance Plated Inline where
  plate f (i', r) = (,r) <$> plateInline' f i'

plateInline' :: Applicative f => (Inline -> f Inline) -> Inline' -> f Inline'
plateInline' f = \case
  IlInlineHolder is -> IlInlineHolder <$> traverse f is
  -- Link is t t' -> Link <$> traverse f is <*> pure t <*> pure t'
  other -> pure other

instance Commonmark.Rangeable Inlines where
  ranged r bs = bs & each %~ (`addRange` r)

instance Commonmark.IsInline Inlines where
  lineBreak = ilNull

  softBreak = ilNull

  str _ = ilNull

  entity _ = ilNull

  escapedChar _ = ilNull

  emph = ilInlineHolder

  strong = ilInlineHolder

  link _ _ = ilInlineHolder

  image _ _ = ilInlineHolder

  code _ = ilNull

  rawInline _ _ = ilNull

instance Extensions.HasEmoji Inlines where
  emoji _ _ = ilNull

instance Extensions.HasStrikethrough Inlines where
  strikethrough = ilInlineHolder

instance Extensions.HasMath Inlines where
  inlineMath _ = ilNull

  displayMath _ = ilNull

instance Extensions.HasQuoted Inlines where
  singleQuoted = ilInlineHolder

  doubleQuoted = ilInlineHolder

instance Extensions.HasSpan Inlines where
  spanWith _ = ilInlineHolder

instance Commonmark.HasAttributes Inlines where
  addAttributes _ = ilInlineHolder

instance Links.HasWikiLink Inlines where
  wikilink conn (url, name) = singleton $ IlWikiLink $ WikiLink {conn, url, name}