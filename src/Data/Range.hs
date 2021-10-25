module Data.Range
  ( Range (start, end),
    length,
  )
where

import Data.Data (Data)
import MyPrelude hiding (length)

data Range = Range
  { start :: !Int,
    end :: !Int
  }
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

length :: Range -> Int
length Range {start, end} = end - start