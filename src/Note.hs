module Note where

-- import Commonmark (ParseError)
-- import Control.Monad.Except
-- import Data.Rope.UTF16 (Rope)
-- import qualified Data.Text as T
-- import Language.LSP.Types (NormalizedFilePath)
-- import qualified Language.LSP.Types as LSP
-- import LineIndex (LineIndex, new)
-- import Markdown.AST (AST)
-- import qualified Markdown.Parsing
-- import MyPrelude
-- import qualified System.FilePath as FilePath

-- data Note = Note
--   { ast :: AST,
--     rope :: Rope,
--     name :: Text,
--     path :: NormalizedFilePath,
--     lineIndex :: LineIndex
--   }
--   deriving (Show, Generic)

-- new :: MonadError ParseError m => LSP.NormalizedFilePath -> Text -> Rope -> m Note
-- new nPath text rope = do
--   let lineIndex = LineIndex.new text
--       path = LSP.fromNormalizedFilePath nPath
--       name = getName path
--   ast <- liftEither $ Markdown.Parsing.parseAST path text
--   pure $ Note {ast, rope, name, path = nPath, lineIndex}

-- getName :: FilePath -> Text
-- getName = T.pack . FilePath.dropExtension . FilePath.takeFileName