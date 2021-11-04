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
import qualified State

data CompletionAction
  = CompleteTags
  | CompleteLinks
  | CompleteNone

getCompletionAction :: Pos -> LSP.CompletionContext -> AST -> Rope -> CompletionAction
getCompletionAction pos cx ast rope =
  if
      | (cx ^. LSP.triggerCharacter & L._Just %~ (== "#")) ^. L.non False -> CompleteTags
      | T.last twoBehind == '#' -> CompleteTags
      | twoBehind == "[[" -> CompleteLinks
      | twoInfront == "]]" -> CompleteLinks
      | Just (AST.LinkElement _, _) <- AST.containingElement (Span.empty pos) ast -> CompleteLinks
      | otherwise -> CompleteNone
  where
    offset = Rope.posToOffset pos rope
    twoBehind = Rope.getRange (Range.new (offset - 2) offset) rope ^. L._Just . Rope.text
    twoInfront = Rope.getRange (Range.new offset (offset + 2)) rope ^. L._Just . Rope.text

handler :: Handlers
handler = requestHandler LSP.STextDocumentCompletion $ \req -> do
  let params = req ^. LSP.params
  -- uri = params ^. LSP.textDocument . LSP.uri . L.to LSP.toNormalizedUri
  note <- getNote params
  let pos = params ^. Proto.pos
      range = Rope.spanToRange (Span.empty pos) (note ^. #rope)
  -- prefix <- VFS.getCompletionPrefix Position VirtualFile
  -- vf <- Server.getVirtualFile uri <&> Unsafe.fromJust

  -- debugM "handlers" $ show params
  let items =
        LSP.List $
          fmap
            (\lab -> def {LSP._label = lab, LSP._kind = Just LSP.CiFile})
            [ "first",
              "second"
            ]
  pure $ LSP.InL items