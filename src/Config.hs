{-# LANGUAGE DeriveAnyClass #-}

module Config
  ( Config (..),
    def,
    LanguageContextEnv,
    MonadLsp,
  )
where

import qualified Data.Aeson as Aeson
import qualified Language.LSP.Server as Server
import MyPrelude

type MonadLsp m = Server.MonadLsp Config m

type LanguageContextEnv = Server.LanguageContextEnv Config

data Config = Config {fooTheBar :: !Bool, wibbleFactor :: !Int}
  deriving (Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

instance Default Config where
  def = Config {fooTheBar = False, wibbleFactor = 0}