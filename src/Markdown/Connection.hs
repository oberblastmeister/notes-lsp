module Markdown.Connection where

import MyPrelude

-- | Represent the connection between zettels
--
-- The order of constructors will reflect the order in backlinks panel (see
-- Links plugin)
data Connection
  = FolgezettelInverse
  | OrdinaryConnection
  | Folgezettel
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Semigroup Connection where
  -- A folgezettel link trumps all other kinds in that zettel.
  FolgezettelInverse <> _ = FolgezettelInverse
  _ <> FolgezettelInverse = FolgezettelInverse
  Folgezettel <> _ = Folgezettel
  _ <> Folgezettel = Folgezettel
  OrdinaryConnection <> OrdinaryConnection = OrdinaryConnection