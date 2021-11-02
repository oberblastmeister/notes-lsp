module Markdown.AST where

import qualified Commonmark
import qualified Control.Lens as L
import Control.Lens.Operators
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Generics.Labels ()
import qualified Data.List.NonEmpty as NE
import Data.Span (Span)
import qualified Data.Span as Span
import qualified Markdown.CST as CST
import Markdown.Connection (Connection)
import MyPrelude
import qualified Relude.Unsafe as Unsafe

type AST = [ASTElement]

type ASTBuilder = DList ASTElement

type ASTElement = (ASTElement', Span)

data ASTElement'
  = LinkElement !WikiLink
  | Header
  deriving (Show, Eq)

data WikiLink = WikiLink
  { conn :: !Connection,
    dest :: !Text,
    name :: !Text
  }
  deriving (Show, Eq, Generic)

instance L.Plated ASTElement where
  plate f (astElem, span) = (,span) <$> plateAstElement' f astElem

plateAstElement' :: Applicative f => (ASTElement -> f ASTElement) -> ASTElement' -> f ASTElement'
plateAstElement' f = \case
  other -> pure other

makeAST :: [CST.Block] -> AST
makeAST = DL.toList . fromBlocks . toList

fromBlocks :: [CST.Block] -> ASTBuilder
fromBlocks = foldMap fromBlock

fromBlock :: CST.Block -> ASTBuilder
fromBlock (bl, _) = case bl of
  CST.BlInlineHolder ils -> fromInlines ils
  CST.BlBlockHolder bls -> fromBlocks bls
  CST.BlList bbls -> foldMap fromBlocks bbls
  CST.BlNull -> mempty

fromInlines :: [CST.Inline] -> ASTBuilder
fromInlines = foldMap fromInline

fromInline :: CST.Inline -> ASTBuilder
fromInline (il, sp) = case il of
  CST.IlInlineHolder ils -> fromInlines ils
  CST.IlWikiLink CST.WikiLink {conn, dest, name} ->
    DL.singleton
      ( LinkElement
          WikiLink
            { conn,
              dest,
              name = dest `fromMaybe` name
            },
        Span.unsafeFromSourceRange $ head $ NE.fromList $ Commonmark.unSourceRange $ Unsafe.fromJust sp
      )
  CST.IlBlockHolder bls -> fromBlocks bls
  CST.IlNull -> mempty

containingElement :: Span -> AST -> Maybe ASTElement
containingElement span =
  find (\(_, span') -> span' `Span.contains` span)
    . reverse -- children before parents
    . L.universeOn L.each