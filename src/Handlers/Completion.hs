{-# LANGUAGE MultiWayIf #-}

module Handlers.Completion (handler) where

import qualified Control.Lens as L
import Control.Lens.Operators
import qualified Data.Maybe as Unsafe
import Data.Pos (Pos)
import qualified Data.Pos as Pos
import qualified Data.Range as Range
import Data.Rope.UTF16 (Rope)
import qualified Data.Rope.Utils as Rope
import qualified Data.Span as Span
import qualified Data.Text as T
import Handlers.Utils
import qualified Language.LSP.Server as Server
import qualified Language.LSP.Types as LSP
import Language.LSP.Types.Instances ()
import qualified Language.LSP.Types.Lens as LSP
import Markdown.AST (AST)
import qualified Markdown.AST as AST
import MyPrelude
import qualified Proto
import State (Note (Note))
import qualified State

handler :: Handlers
handler = requestHandler LSP.STextDocumentCompletion $ \req -> do
  -- error ""
  let params = req ^. LSP.params
      cx = params ^. LSP.context
      pos = params ^. Proto.pos
  -- uri = params ^. LSP.textDocument . LSP.uri . L.to LSP.toNormalizedUri
  note@Note {ast, rope} <- getNote params
  let pos = params ^. Proto.pos
  let res = LSP.InL $ case getCompletionAction pos cx ast rope of
        CompleteLinks -> completeLinks
        CompleteTags -> completeTags
        CompleteNone -> mempty
  pure res

-- pure $ LSP.InL mempty

completeLinks :: LSP.List LSP.CompletionItem
completeLinks =
  LSP.List $
    fmap
      (\lab -> def {LSP._label = lab, LSP._kind = Just LSP.CiFile})
      [ "first",
        "second"
      ]

completeTags :: LSP.List LSP.CompletionItem
completeTags = mempty

data CompletionAction
  = CompleteTags
  | CompleteLinks
  | CompleteNone

getCompletionAction :: Pos -> Maybe LSP.CompletionContext -> AST -> Rope -> CompletionAction
getCompletionAction pos cx ast rope =
  if
      | Just LSP.CompletionContext {LSP._triggerCharacter = Just "#"} <- cx -> CompleteTags
      | Just t <- twoBehind, T.last t == '#' -> CompleteTags
      | Just "[[" <- twoBehind -> CompleteLinks
      | Just "]]" <- twoInfront -> CompleteLinks
      | Just (AST.LinkElement _, _) <- AST.containingElement (Span.empty pos) ast -> CompleteLinks
      | otherwise -> CompleteNone
  where
    offset = Rope.posToOffset pos rope
    twoBehind =
      if offset - 2 > 0
        then Rope.getRange (Range.new (offset - 2) offset) rope ^? L._Just . Rope.text
        else Nothing
    twoInfront = Rope.getRange (Range.new offset (offset + 2)) rope ^? L._Just . Rope.text