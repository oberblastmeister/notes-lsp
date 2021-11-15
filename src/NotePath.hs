module NotePath where

import MyPrelude

newtype NotePath = NotePath {path :: FilePath}
  deriving (Show, Eq, Ord, Generic)

newtype Component = Component {unComponent :: Text}
  deriving (Show, Eq, Ord, Generic)

-- fromFilePath :: FilePath -> FilePath -> Maybe NotePath
-- fromFilePath root path = do
