module Data.Conduit.Combinators.Extra (listDirRec) where

import Conduit (ConduitT, MonadResource, (.|))
import qualified Conduit
import qualified Data.Conduit.Combinators as C
import MyPrelude
import System.FilePath ((</>))
import qualified UnliftIO.Directory as Directory

listDirRec :: (MonadResource m) => FilePath -> ConduitT i FilePath m ()
listDirRec path = do
  path' <- Directory.makeAbsolute path
  Conduit.sourceDirectory path'
    .| C.map (path </>)
    .| Conduit.awaitForever
      ( \entry -> do
          Conduit.yield entry
          whenM (Directory.doesDirectoryExist entry) $ listDirRec entry
      )