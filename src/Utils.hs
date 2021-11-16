module Utils
  ( scanl'',
    forMaybe,
    forMaybeM,
    listDirFilesIgnore,
    findUntil,
    fromJustMsg,
    intoException,
    async',
    fromLeft',
    fromRight',
    fromRightShow',
  )
where

import Conduit (ConduitT, MonadResource, awaitForever, runConduit, runResourceT, yield, (.|))
import qualified Conduit
import qualified Data.Conduit.Combinators as C
import MyPrelude
import System.FilePath ((</>))
import UnliftIO (Async, async)
import qualified UnliftIO.Concurrent as Concurrent
import qualified UnliftIO.Directory as Directory
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

isHidden :: FilePath -> Bool
isHidden = isPrefixOf "."

listDirFilesIgnore :: (MonadUnliftIO m) => FilePath -> m [FilePath]
listDirFilesIgnore path = runResourceT $ runConduit $ listDirRec path .| C.sinkList

listDirRec :: (MonadResource m) => FilePath -> ConduitT i FilePath m ()
listDirRec path =
  Conduit.sourceDirectory path
    .| C.map (path </>)
    .| awaitForever
      ( \entry -> do
          isDir <- Directory.doesDirectoryExist entry
          if isDir
            then listDirRec entry
            else yield entry
      )

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

-- test :: ghc-prim-0.6.1:GHC.Types.Any a
-- -> ghc-prim-0.6.1:GHC.Types.Any
--      (async-2.2.4:Control.Concurrent.Async.Async a)
-- test = async

-- | async but will propagate exceptions to the parent thread
-- | This means that the monadic action cannot return anything
-- | to the parent thread.
async' :: MonadUnliftIO m => m () -> m (Async ())
async' m = do
  tid <- Concurrent.myThreadId
  async $
    Exception.catchSyncOrAsync
      @_
      @SomeException
      m
      (Concurrent.throwTo tid)

fromLeft' :: HasCallStack => Either a b -> a
fromLeft' (Left a) = a
fromLeft' (Right _b) = error "Utils.fromLeft': Got right"

fromRight' :: HasCallStack => Either a b -> b
fromRight' (Right b) = b
fromRight' (Left _a) = error "Utils.fromRight': Got left but expected right"

fromRightShow' :: (HasCallStack, Show a) => Either a b -> b
fromRightShow' (Right b) = b
fromRightShow' (Left a) = error $ "Utils.fromRight': Expected right but got " <> show a