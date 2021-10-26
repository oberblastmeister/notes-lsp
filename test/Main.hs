module Main (main) where

import MyPrelude
import qualified LineIndex
import qualified Spec
import Control.Lens
import Data.Data (Data)
import Data.IxSet.Typed (IxSet, (@<), (@=))
import qualified Data.IxSet.Typed as IxSet
import qualified Markdown.Parsing
import MyPrelude
import Text.Pretty.Simple
import qualified Prelude as P


main :: IO ()
main = do
  -- runIndexSet
  -- Spec.main
  -- parseTree
  runGroupBy
  return ()
  
runGroupBy :: IO ()
runGroupBy = do
  print $ LineIndex.addCol $ LineIndex.addCharLen "adfadf😋"

parseTree :: IO ()
parseTree = do
  let s =
        "# this is a title adpsofiuasdf\n\
        \**emphatic**\n\
        \*italics*\n\
        \"
  let s =
        "# hi\n\
        \[[adsfasdf]]"
  let s = "\tasdf\t"
  let s = "a𐐀b"
  case Markdown.Parsing.parseMarkdown "<none>" s of
    Left e -> error $ show e
    Right bs -> pPrint bs

data Entry = Entry
  { author :: Author,
    authors :: [Author],
    updated :: Updated,
    id :: Id,
    content :: Content
  }
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

newtype Updated = Updated Int
  deriving (Show, Eq, Ord, Data, Typeable)

newtype Id = Id Int64
  deriving (Show, Eq, Ord, Data, Typeable)

newtype Content = Content String
  deriving (Show, Eq, Ord, Data, Typeable)

newtype Author = Author Email
  deriving (Show, Eq, Ord, Data, Typeable)

type Email = String

data Test = Test
  deriving (Show, Eq, Ord, Data, Typeable)

type EntryIxs = '[Author, [Author], Updated, Id, Content]

type IxEntry = IxSet EntryIxs Entry

instance IxSet.Indexable EntryIxs Entry where
  indices =
    IxSet.ixList
      (IxSet.ixFun (one . (^. #author)))
      (IxSet.ixFun (one . (^. #authors)))
      (IxSet.ixFun (one . (^. #updated)))
      (IxSet.ixFun (one . (^. #id)))
      (IxSet.ixFun (one . (^. #content)))

runIndexSet :: IO ()
runIndexSet = do
  let e1 = Entry (Author "brian@gmail.com") [] (Updated 1) (Id 1234) (Content "hello world!")
      e2 = Entry (Author "you@gmail.com") [] (Updated 3) (Id 1235) (Content "another content")
      e3 = Entry (Author "broh@gmail.com") [] (Updated 10) (Id 1236) (Content "broh")
  -- e3 = Author "another@gmail.com"
  -- e4 = Author "you@gmail.com"
  let entries = IxSet.insertList [e1, e2, e3] (IxSet.empty :: IxEntry)
  -- let entries1 = foldr IxSet.delete entries [e1, e3]
  let res = entries @= [] @Author
  putStrLn "found:"
  print $ IxSet.toList res
  return ()