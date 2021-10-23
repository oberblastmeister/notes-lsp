module Markdown.Tags where

import qualified Commonmark
import qualified Commonmark.Inlines
import MyPrelude
import qualified Text.Parsec as P

-- class HasTag a where
--   tag :: a

-- inlineTagSpec ::
--   (Monad m, Commonmark.IsBlock il bl, Commonmark.IsInline il) =>
--   Commonmark.SyntaxSpec m il bl
-- inlineTagSpec =
--   mempty
--     { Commonmark.syntaxInlineParsers = [pInlineTag]
--     }
--   where
--     pInlineTag ::
--       (Monad m, Commonmark.IsInline il) =>
--       Commonmark.Inlines.InlineParser m il
--     pInlineTag = P.try $ do
--       _ <- symbol '#'
--       tag <- Commonmark.untokenize <$> inlineTagP
--       case makeZTagURI tag of
--         Nothing ->
--           fail "Not an inline tag"
--         Just (URI.render -> url) ->
--           pure $! cmAutoLink OrdinaryConnection url
--     makeZTagURI :: Text -> Maybe URI
--     makeZTagURI s = do
--       tag <- URI.mkPathPiece "tag"
--       path <- traverse URI.mkPathPiece $ T.splitOn "/" s
--       pure $ URI.URI (Just [scheme|z|]) (Left False) (Just (False, tag :| path)) [] Nothing