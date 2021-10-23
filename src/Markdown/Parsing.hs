module Markdown.Parsing
  ( SyntaxSpec,
    SyntaxSpec',
    syntaxSpec,
  )
where

import qualified Commonmark
import qualified Commonmark.Extensions
import qualified Commonmark.Pandoc
import qualified Markdown.CST as CST
import MyPrelude
import qualified Text.Pandoc.Builder as Pandoc.Builder
import qualified Text.Parsec as Parsec

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
    Commonmark.Extensions.HasSpan il
  )

syntaxSpec ::
  SyntaxSpec' m il bl =>
  Commonmark.SyntaxSpec m il bl
syntaxSpec =
  -- TODO: Move the bulk of markdown extensions to a neuron extension
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
      Commonmark.Extensions.pipeTableSpec
    ]

parseMarkdown :: FilePath -> Text -> ()
parseMarkdown path input = do
  let _ = case join $ Commonmark.commonmarkWith @_ @CST.Inlines @CST.Blocks syntaxSpec path input of
        Left pe -> undefined
        Right it -> it
  undefined