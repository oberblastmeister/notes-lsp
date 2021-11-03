module Logging
  ( debugM,
    infoM,
    noticeM,
    warningM,
    errorM,
  )
where

import MyPrelude
import qualified System.Log.Logger as Logger

liftLogger :: MonadIO m => (String -> String -> IO ()) -> String -> String -> m ()
liftLogger f label msg = liftIO $ f label msg

debugM :: MonadIO m => String -> String -> m ()
debugM = liftLogger Logger.debugM

infoM :: MonadIO m => String -> String -> m ()
infoM = liftLogger Logger.infoM

noticeM :: MonadIO m => String -> String -> m ()
noticeM = liftLogger Logger.noticeM

warningM :: MonadIO m => String -> String -> m ()
warningM = liftLogger Logger.warningM

errorM :: MonadIO m => String -> String -> m ()
errorM = liftLogger Logger.errorM