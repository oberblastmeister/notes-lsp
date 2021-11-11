{-# LANGUAGE DeriveAnyClass #-}

module Config
  ( Config (..),
    def,
    LanguageContextEnv,
    MonadLsp,
    ServerDefinition,
  )
where

import qualified Data.Aeson as Aeson
import qualified Language.LSP.Server as Server
import MyPrelude

type MonadLsp m = Server.MonadLsp Config m

type LanguageContextEnv = Server.LanguageContextEnv Config

type ServerDefinition = Server.ServerDefinition Config

data Config = Config
  { waitInitializeState :: !Bool
  }
  deriving (Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

instance Default Config where
  def =
    Config
      { waitInitializeState = False
      }