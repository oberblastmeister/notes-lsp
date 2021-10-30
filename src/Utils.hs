module Utils
  ( scanl'',
    forMaybe,
    forMaybeM,
  )
where

import MyPrelude

scanl'' :: (b -> a -> b) -> b -> [a] -> [b]
scanl'' f z = drop 1 . scanl' f z

forMaybe :: [a] -> (a -> Maybe b) -> [b]
forMaybe = flip mapMaybe

forMaybeM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMaybeM = flip mapMaybeM