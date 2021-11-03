module Utils
  ( scanl'',
    forMaybe,
    forMaybeM,
    listDirFilesIgnore,
    findUntil,
    fromJustMsg,
    intoException,
  )
where

import MyPrelude
import qualified Path.IO as PIO
import qualified UnliftIO.Exception as Exception

scanl'' :: (b -> a -> b) -> b -> [a] -> [b]
scanl'' f z = drop 1 . scanl' f z

findUntil :: (a -> Bool) -> [a] -> Maybe a
findUntil fn list = case list of
  [] -> Nothing
  (hd : tl) -> go hd fn tl
  where
    go _prev _f [] = Nothing
    go prev f (x : xs) = if f x then Just prev else go x f xs

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

fromJustMsg :: (MonadIO m, HasCallStack) => String -> Maybe a -> m a
fromJustMsg msg = \case
  Just a -> pure a
  Nothing -> Exception.throwString msg

intoException :: (Show e, MonadIO m) => ExceptT e m a -> m a
intoException m = do
  res <- runExceptT m
  case res of
    Left e -> Exception.throwString $ show e
    Right x -> pure x