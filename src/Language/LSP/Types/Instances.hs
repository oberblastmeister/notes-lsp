{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.LSP.Types.Instances where

import Data.Default
import qualified Language.LSP.Types as LSP
import MyPrelude

instance Default LSP.CompletionItem where
  def =
    LSP.CompletionItem
      { _label = "",
        _kind = Nothing,
        _tags = Nothing,
        _detail = Nothing,
        _documentation = Nothing,
        _deprecated = Nothing,
        _preselect = Nothing,
        _sortText = Nothing,
        _filterText = Nothing,
        _insertText = Nothing,
        _insertTextFormat = Nothing,
        _insertTextMode = Nothing,
        _textEdit = Nothing,
        _additionalTextEdits = Nothing,
        _commitCharacters = Nothing,
        _command = Nothing,
        _xdata = Nothing
      }