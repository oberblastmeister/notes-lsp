{-# LANGUAGE DeriveAnyClass #-}

module LinkPath where

import Control.Lens.Operators
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Normalize as Text.Normalize
import MyPrelude
import qualified System.FilePath as Path

data LinkPath = LinkPath
  { kind :: PathKind,
    components :: NonEmpty Component
  }
  deriving (Show, Eq, Generic, Hashable)

data PathKind = NotePath | OtherPath
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

newtype Component = Component {unComponent :: Text}
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

componentFromText :: Text -> Component
componentFromText (toText -> s) =
  if "/" `T.isInfixOf` s
    then error ("Slug cannot contain a slash: " <> s)
    else Component $ unicodeNormalize s

fromPath :: FilePath -> FilePath -> Maybe LinkPath
fromPath root path = LinkPath kind <$> nonEmpty components
  where
    components = componentFromText . T.dropWhileEnd (== '/') . toText <$> Path.splitPath path''
    (kind, path'') =
      maybe
        (OtherPath, path')
        (NotePath,)
        (Path.stripExtension ".md" path')
    path' = Path.makeRelative root path

unicodeNormalize :: Text -> Text
unicodeNormalize = Text.Normalize.normalize Text.Normalize.NFC

newtype WikiLink = WikiLink {components :: NonEmpty Component}
  deriving (Show, Eq, Generic)
  deriving anyclass (Hashable)

allowedWikiLinks :: LinkPath -> NonEmpty WikiLink
allowedWikiLinks linkPath =
  NE.fromList
    . mapMaybe
      ((WikiLink <$>) . nonEmpty)
    . tails
    . NE.toList
    $ linkPath ^. #components