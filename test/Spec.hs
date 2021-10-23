module Spec where

import qualified Commonmark
import qualified Commonmark.Extensions
import qualified Commonmark.Extensions.Wikilinks as Commonmark
import qualified Commonmark.Pandoc
import Commonmark.Parser (commonmark)
import Control.Monad (join)
import Control.Monad.Identity (runIdentity)
import Data.Either.Extra
import Data.Text (Text)
import Data.Typeable (Typeable)
import Markdown.CST (Blocks)
import Text.Pandoc.Builder (Pandoc (Pandoc))
import qualified Text.Pandoc.Builder as Pandoc.Builder
import Text.Pretty.Simple

main :: IO ()
main = do
  -- parseSourceMap
  parseTree
  return ()

run :: IO ()
run = do
  let doc =
        parseMarkdown
          "<none>"
          $ "hi [[link]]"
            <> "\n|adfad|adsfad|\n\
               \|-----|------|\n\
               \ $$whatishti$$\n\
               \- [ ] Task"
  pPrint doc

parseTree :: IO ()
parseTree = do
  let s = "# this is a title adpsofiuasdf\n\
  \"
  case Commonmark.commonmark "<none>" s of
    Left e -> error $ show e
    Right (bs :: Blocks) -> pPrint bs

parseSourceMap :: IO ()
parseSourceMap =
  case Commonmark.runWithSourceMap
    <$> runIdentity
      ( Commonmark.commonmarkWith
          Commonmark.defaultSyntaxSpec
          "<none>"
          "# hello world"
      ) of
    Left e -> error $ show e
    Right (_ :: Commonmark.Html (), sm) -> pPrint sm

parseMarkdown ::
  NeuronSyntaxSpec m il bl =>
  FilePath ->
  Text ->
  Pandoc
parseMarkdown path markdown = do
  let v =
        fromRight' $
          commonmarkPandocWith
            ( Commonmark.defaultSyntaxSpec
                <> Commonmark.Extensions.wikilinksSpec Commonmark.Extensions.TitleAfterPipe
                <> Commonmark.Extensions.pipeTableSpec
                <> Commonmark.Extensions.mathSpec
            )
            path
            markdown
  let doc = Pandoc mempty $ Pandoc.Builder.toList $ Commonmark.Pandoc.unCm v
  doc

commonmarkPandocWith ::
  NeuronSyntaxSpec m il bl =>
  Commonmark.SyntaxSpec m il bl ->
  FilePath ->
  Text ->
  m bl
commonmarkPandocWith spec path s =
  join $ Commonmark.commonmarkWith spec path s

type NeuronSyntaxSpec m il bl =
  ( NeuronSyntaxSpec' m il bl,
    m ~ Either Commonmark.ParseError,
    bl ~ Commonmark.Pandoc.Cm Commonmark.SourceRange Pandoc.Builder.Blocks
  )

type NeuronSyntaxSpec' m il bl =
  ( Monad m,
    Commonmark.IsBlock il bl,
    Commonmark.IsInline il,
    Commonmark.HasWikilinks il
    -- CE.HasEmoji il,
    -- CE.HasStrikethrough il,
    -- CE.HasPipeTable il bl,
    -- CE.HasTaskList il bl,
    -- CM.ToPlainText il,
    -- CE.HasFootnote il bl,
    -- CE.HasMath il,
    -- CE.HasDefinitionList il bl,
    -- CE.HasDiv bl,
    -- CE.HasQuoted il,
    -- CE.HasSpan il,
    -- HasHighlight il
  )