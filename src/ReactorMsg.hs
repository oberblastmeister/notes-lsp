module ReactorMsg where

import qualified Config
import MyPrelude
import State (ServerM, ServerState)

data ReactorMsg
  = ReactorMsgAct ReactorAct
  | ReactorMsgInitState ServerState

data ReactorAct = ReactorAct
  { act :: ServerM (),
    env :: Config.LanguageContextEnv
  }

type MonadReactor :: (* -> *) -> Constraint

type MonadReactor m = (MonadReader (IORef ServerState) m, MonadIO m)

type ReactorM = ReaderT (IORef ServerState) IO ()
