{-# LANGUAGE TemplateHaskell #-}

module Data.IdMap
  ( Id(unId),
    IdMap,
    insert,
    delete,
    adjust,
    unsafeId,
    lookup,
    update,
    atId,
  )
where

import qualified Control.Lens as L
import Control.Lens.Operators
import Data.Data (Data)
import qualified Data.IntMap.Strict as IntMap
import MyPrelude
import Control.Lens (Lens')

newtype Id a = UnsafeId {unId :: Int}
  deriving (Show, Eq, Ord, Data, Typeable)

data IdMap a = IdMap
  { _intMap :: !(IntMap a),
    _holes :: ![Int],
    _nextId :: !Int
  }
  deriving (Show, Eq, Data, Typeable)

L.makeLenses ''IdMap

unsafeId :: Int -> Id a
unsafeId = UnsafeId

insert :: a -> IdMap a -> (Id a, IdMap a)
insert a im = case im ^. holes of
  hole : _holes -> (UnsafeId hole, im & intMap . L.at hole ?~ a)
  [] -> (im ^. nextId . L.to UnsafeId, im & intMap . L.at (im ^. nextId) ?~ a & nextId +~ 1)

delete :: Id a -> IdMap a -> IdMap a
delete (UnsafeId i) im =
  if isJust prev
    then im' & holes %~ (i :)
    else im'
  where
    (prev, im') = im & intMap . L.at i <<.~ Nothing

adjust :: Id a -> (a -> a) -> IdMap a -> IdMap a
adjust (UnsafeId i) f im = im & intMap . L.ix i %~ f

lookup :: Id a -> IdMap a -> a
lookup (UnsafeId i) im = im ^?! intMap . L.ix i

update :: Id a -> a -> IdMap a -> IdMap a
update (UnsafeId i) val im = im & intMap . L.at i ?~ val

atId :: Id a -> Lens' (IdMap a) a
atId i = L.lens (lookup i) (flip $ update i)