module Markdown.AST where

import MyPrelude
import qualified Commonmark
import Data.DList (DList)
import Data.Generics.Labels ()

type AST = [ASTElement]

type ASTBuilder = DList ASTElement

type ASTElement = (ASTElement', Commonmark.SourceRange)

data ASTElement'
  = LinkElement Link
  | Header
  deriving (Show, Eq)

data Link = Link
  { inner :: [ASTElement],
    name :: !Text,
    dest :: !Text
  }
  deriving (Show, Eq, Generic)

-- fromBlocks :: Blocks -> AST
-- fromBlocks = \case
-- CST.InlineHolder
--
-- fromInlines :: [CST.Inline] -> [CST.Inline]
-- fromInlines ils = filter (hasn't #_IlNull) ils

-- fromInline :: CST.Inline -> ASTElement
-- fromInline = \case
--   Link