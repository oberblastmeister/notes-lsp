module Markdown.Parsing
  ( SyntaxSpec,
    SyntaxSpec',
    syntaxSpec,
    parseCST,
    parseAST,
  )
where

import Commonmark (ParseError)
import qualified Commonmark
import qualified Commonmark.Extensions
import qualified Commonmark.Pandoc
import Markdown.AST (AST)
import qualified Markdown.AST as AST
import qualified Markdown.CST as CST
import qualified Markdown.Links as Links
import MyPrelude
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Pandoc.Builder as Pandoc.Builder
import qualified Text.Parsec as Parsec
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml

type SyntaxSpec m il bl =
  ( SyntaxSpec' m il bl,
    m ~ Either Parsec.ParseError,
    bl ~ Commonmark.Pandoc.Cm () Pandoc.Builder.Blocks
  )

type SyntaxSpec' m il bl =
  ( Monad m,
    Commonmark.IsBlock il bl,
    Commonmark.IsInline il,
    Typeable m,
    Typeable il,
    Typeable bl,
    Commonmark.Extensions.HasEmoji il,
    Commonmark.Extensions.HasStrikethrough il,
    Commonmark.Extensions.HasPipeTable il bl,
    Commonmark.Extensions.HasTaskList il bl,
    Commonmark.Extensions.HasFootnote il bl,
    Commonmark.Extensions.HasMath il,
    -- Commonmark.Extensions.HasDefinitionList il bl,
    Commonmark.Extensions.HasDiv bl,
    Commonmark.Extensions.HasQuoted il,
    Commonmark.Extensions.HasSpan il,
    Links.HasWikiLink il
  )

syntaxSpec ::
  SyntaxSpec' m il bl =>
  Commonmark.SyntaxSpec m il bl
syntaxSpec =
  mconcat
    [ Commonmark.Extensions.fancyListSpec,
      Commonmark.Extensions.footnoteSpec,
      Commonmark.Extensions.mathSpec,
      Commonmark.Extensions.smartPunctuationSpec,
      -- Commonmark.Extensions.definitionListSpec,
      Commonmark.Extensions.attributesSpec,
      Commonmark.Extensions.rawAttributeSpec,
      Commonmark.Extensions.fencedDivSpec,
      Commonmark.Extensions.bracketedSpanSpec,
      Commonmark.Extensions.autolinkSpec,
      Commonmark.defaultSyntaxSpec,
      -- as the commonmark documentation states, pipeTableSpec should be placed after
      -- fancyListSpec and defaultSyntaxSpec to avoid bad results when non-table lines
      Commonmark.Extensions.pipeTableSpec,
      Links.wikiLinkSpec
    ]

parseAST :: FilePath -> Text -> Either Text AST
parseAST path input = uncurry AST.makeAST <$> parseCST path input

parseCST :: forall meta. (Aeson.FromJSON meta) => FilePath -> Text -> Either Text (Maybe meta, [CST.Block])
parseCST path input = do
  (mMeta, markdown) <- partitionMarkdown path input
  mMetaVal <- first show $ (Yaml.decodeEither' . encodeUtf8) `traverse` mMeta
  cst <- bimap
    show
    toList
    ( runIdentity
        ( Commonmark.commonmarkWith
            @Identity
            @CST.Inlines
            @CST.Blocks
            syntaxSpec
            path
            markdown
        )
    )
  pure (mMetaVal, cst)

partitionMarkdown :: FilePath -> Text -> Either Text (Maybe Text, Text)
partitionMarkdown =
  parse (M.try splitP <|> fmap (Nothing,) M.takeRest)
  where
    separatorP :: M.Parsec Void Text ()
    separatorP =
      void $ M.string "---" <* M.eol

    splitP :: M.Parsec Void Text (Maybe Text, Text)
    splitP = do
      separatorP
      a <- toText <$> M.manyTill M.anySingle (M.try $ M.eol *> separatorP)
      b <- M.takeRest
      pure (Just a, b)

    parse :: M.Parsec Void Text a -> String -> Text -> Either Text a
    parse p fn s =
      first (toText . M.errorBundlePretty) $
        M.parse (p <* M.eof) fn s