module Markdown.CST where

import qualified Commonmark
import Control.Lens.Operators
import qualified Commonmark.Extensions as Extensions
import qualified Control.Lens as L
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

ilBlockHolder :: Blocks -> Inlines
ilBlockHolder = singleton . IlBlockHolder . toList

addRange ::
  (Show a, HasCallStack) =>
  (a, Maybe Commonmark.SourceRange) ->
  Commonmark.SourceRange ->
  (a, Maybe Commonmark.SourceRange)
addRange old pos =
  let !_ =
        if isJust (old ^. L._2)
          then
            -- error $ "setting a span range for: " <> show old <> "\nwith span: " <> show pos
            ()
          else ()
   in old & L._2 ?~ pos

type Block = (Block', Maybe Commonmark.SourceRange)

data Block'
  = BlInlineHolder [Inline]
  | BlBlockHolder [Block]
  | BlList [[Block]]
  | -- | DefinitionList [([Inline], [Block])]
    BlNull
  deriving (Show, Eq, Generic)

type Blocks = Seq Block

instance L.Plated Block where
  plate f (b', r) = (,r) <$> plateBlock' f b'

plateBlock' :: Applicative f => (Block -> f Block) -> Block' -> f Block'
plateBlock' f = \case
  BlBlockHolder bs -> BlBlockHolder <$> traverse f bs
  other -> pure other

instance Commonmark.Rangeable Blocks where
  ranged r bs = bs & L.each %~ (`addRange` r)

instance Commonmark.HasAttributes Blocks where
  addAttributes _ = blBlHolder

instance Commonmark.IsBlock Inlines Blocks where
  paragraph = inLineHolder

  plain = inLineHolder

  thematicBreak = blNull

  blockQuote = blBlHolder

  codeBlock _ _ = blNull

  heading _ = inLineHolder

  rawBlock _ _ = blNull

  referenceLinkDefinition _ _ = blNull

  list _ _ = singleton . BlList . map toList

instance Extensions.HasDiv Blocks where
  div_ = blBlHolder

instance Extensions.HasPipeTable Inlines Blocks where
  pipeTable _ ils ilss = singleton $ BlInlineHolder $ concatMap toList ils ++ concatMap (concatMap toList) ilss

instance Extensions.HasTaskList Inlines Blocks where
  taskList _ _ stuff = stuff ^.. L.each . L._2 . L.each ^. L.to (singleton . BlBlockHolder)

instance Extensions.HasFootnote Inlines Blocks where
  footnote _ _ = blBlHolder

  footnoteList = singleton . BlList . map toList

  footnoteRef _ _ = ilBlockHolder

-- instance Extensions.HasDefinitionList Inlines Blocks where
-- definitionList _ = _

type Inline = (Inline', Maybe Commonmark.SourceRange)

data Inline'
  = IlNull
  | IlInlineHolder [Inline]
  | IlWikiLink WikiLink
  | IlBlockHolder [Block]
  deriving (Show, Eq, Generic)

data WikiLink = WikiLink
  { conn :: !Connection,
    dest :: !Text,
    name :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

type Inlines = Seq Inline

instance L.Plated Inline where
  plate f (i', r) = (,r) <$> plateInline' f i'

plateInline' :: Applicative f => (Inline -> f Inline) -> Inline' -> f Inline'
plateInline' f = \case
  IlInlineHolder is -> IlInlineHolder <$> traverse f is
  -- Link is t t' -> Link <$> traverse f is <*> pure t <*> pure t'
  other -> pure other

instance Commonmark.Rangeable Inlines where
  ranged r bs = bs & L.each %~ (`addRange` r)

instance Commonmark.IsInline Inlines where
  lineBreak = ilNull
  {-# INLINE lineBreak #-}

  softBreak = ilNull
  {-# INLINE softBreak #-}

  str _ = ilNull
  {-# INLINE str #-}

  entity _ = ilNull
  {-# INLINE entity #-}

  escapedChar _ = ilNull
  {-# INLINE escapedChar #-}

  emph = ilInlineHolder
  {-# INLINE emph #-}

  strong = ilInlineHolder
  {-# INLINE strong #-}

  link _ _ = ilInlineHolder
  {-# INLINE link #-}

  image _ _ = ilInlineHolder
  {-# INLINE image #-}

  code _ = ilNull
  {-# INLINE code #-}

  rawInline _ _ = ilNull
  {-# INLINE rawInline #-}

instance Extensions.HasEmoji Inlines where
  emoji _ _ = ilNull
  {-# INLINE emoji #-}

instance Extensions.HasStrikethrough Inlines where
  strikethrough = ilInlineHolder
  {-# INLINE strikethrough #-}

instance Extensions.HasMath Inlines where
  inlineMath _ = ilNull
  {-# INLINE inlineMath #-}

  displayMath _ = ilNull
  {-# INLINE displayMath #-}

instance Extensions.HasQuoted Inlines where
  singleQuoted = ilInlineHolder
  {-# INLINE singleQuoted #-}

  doubleQuoted = ilInlineHolder
  {-# INLINE doubleQuoted #-}

instance Extensions.HasSpan Inlines where
  spanWith _ = ilInlineHolder
  {-# INLINE spanWith #-}

instance Commonmark.HasAttributes Inlines where
  addAttributes _ = ilInlineHolder
  {-# INLINE addAttributes #-}

instance Links.HasWikiLink Inlines where
  wikilink conn (dest, name) = singleton $ IlWikiLink $ WikiLink {conn, dest, name}