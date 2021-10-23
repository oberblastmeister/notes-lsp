module Config
  ( Config (..),
    def,
    LanguageContextEnv,
    MonadLsp,
  )
where

import MyPrelude
import qualified Data.Aeson as Aeson
import qualified Language.LSP.Server as Server

type MonadLsp m = Server.MonadLsp Config m

type LanguageContextEnv = Server.LanguageContextEnv Config

data Config = Config {fooTheBar :: !Bool, wibbleFactor :: !Int}
  deriving (Generic, Show)

instance Aeson.ToJSON Config

instance Aeson.FromJSON Config

def :: Config
def = Config {fooTheBar = False, wibbleFactor = 0}