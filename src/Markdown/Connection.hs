module Markdown.Connection where

import MyPrelude
import qualified Text.Read as Read
import qualified Text.Show as Show

-- | Represent the connection between zettels
--
-- The order of constructors will reflect the order in backlinks panel (see
-- Links plugin)
data Connection
  = FolgezettelInverse
  | OrdinaryConnection
  | Folgezettel
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance Semigroup Connection where
  -- A folgezettel link trumps all other kinds in that zettel.
  FolgezettelInverse <> _ = FolgezettelInverse
  _ <> FolgezettelInverse = FolgezettelInverse
  Folgezettel <> _ = Folgezettel
  _ <> Folgezettel = Folgezettel
  OrdinaryConnection <> OrdinaryConnection = OrdinaryConnection

instance Show Connection where
  show = \case
    Folgezettel -> "folge"
    FolgezettelInverse -> "folgeinv"
    OrdinaryConnection -> "cf"

instance Read Connection where
  readsPrec _ s
    | s == show Folgezettel = [(Folgezettel, "")]
    | s == show FolgezettelInverse = [(FolgezettelInverse, "")]
    | s == show OrdinaryConnection = [(OrdinaryConnection, "")]
    | otherwise = []