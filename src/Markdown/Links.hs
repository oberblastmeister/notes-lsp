module Markdown.Links where

import qualified Commonmark
import qualified Commonmark.Inlines
import Commonmark.TokParsers (noneOfToks, symbol)
import qualified Data.Text as T
import Markdown.Connection (Connection)
import MyPrelude
import qualified Text.Megaparsec as M
import qualified Text.Parsec as P
import qualified Markdown.Connection as Connection
import Data.List.Split (wordsBy)

wikiLinkSpec ::
  (Monad m, Commonmark.IsBlock il bl, Commonmark.IsInline il) =>
  Commonmark.SyntaxSpec m il bl
wikiLinkSpec =
  mempty
    { Commonmark.syntaxInlineParsers = [pLink]
    }
  where
    pLink ::
      (Monad m, Commonmark.IsInline il) =>
      Commonmark.Inlines.InlineParser m il
    pLink =
      P.try $
        P.choice
          [ -- Folgezettel links
            cmAutoLink Connection.Folgezettel <$> P.try (wikiLinkP 3),
            cmAutoLink Connection.FolgezettelInverse <$> P.try (symbol '#' *> wikiLinkP 2),
            cmAutoLink Connection.Folgezettel <$> P.try (wikiLinkP 2 <* symbol '#'),
            -- Cf link: [[...]]
            cmAutoLink Connection.OrdinaryConnection <$> P.try (wikiLinkP 2)
          ]
    wikiLinkP :: Monad m => Int -> P.ParsecT [Commonmark.Tok] s m (Text, Maybe Text)
    wikiLinkP n = do
      void $ M.count n $ symbol '['
      toks <- some $ noneOfToks [Commonmark.Symbol ']', Commonmark.Symbol '[', Commonmark.LineEnd]
      void $ M.count n $ symbol ']'
      let parts = Commonmark.untokenize <$> wordsBy (\x -> Commonmark.tokType x == Commonmark.Symbol '|') toks
      case parts of
        [url] -> pure (url, Nothing)
        [url, name] -> pure (url, Just name)
        [] -> fail "Empty wiki-link encountered"
        _ -> fail ("Multiple pipe ('|') characters encountered; here are the parts: " ++ T.unpack (T.intercalate ", " parts))
    cmAutoLink :: Commonmark.IsInline a => Connection -> (Text, Maybe Text) -> a
    cmAutoLink conn (url, name) =
      Commonmark.link url title $ Commonmark.str (url `fromMaybe` name)
      where
        -- Store connetion type in 'title' attribute
        -- TODO: Put it in attrs instead; requires PR to commonmark
        title = show conn
