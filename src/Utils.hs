module Utils
  ( scanl'',
    forMaybe,
    forMaybeM,
    listDirFilesIgnore,
  )
where

import MyPrelude
import qualified Path.IO as PIO

scanl'' :: (b -> a -> b) -> b -> [a] -> [b]
scanl'' f z = drop 1 . scanl' f z

forMaybe :: [a] -> (a -> Maybe b) -> [b]
forMaybe = flip mapMaybe

forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM = flip mapMaybeM

isHidden :: Path b t -> Bool
isHidden = isPrefixOf "." . toFilePath

listDirFilesIgnore :: MonadIO m => Path b Dir -> m [Path Abs File]
listDirFilesIgnore =
  PIO.walkDirAccum
    (Just (\_dir subdirs _files -> filter isHidden subdirs & PIO.WalkExclude & pure))
    (\_dir _subdirs files -> filter (not . isHidden) files & pure)