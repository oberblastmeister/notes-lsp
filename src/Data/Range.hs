module Data.Range
  ( Range (start, end, RangeV),
    length,
    unsafeNew,
    new,
    _start,
    _end,
    tuple,
  )
where

import Control.Lens (Getter, Iso', from, iso, to, view, (^.))
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import MyPrelude hiding (length)

data Range = Range
  { start :: !Int,
    end :: !Int
  }
  deriving (Show, Eq, Ord)

instance Unbox Range

newtype instance VUM.MVector s Range = MV_Range (VU.MVector s (Int, Int))

newtype instance VU.Vector Range = V_Range (VU.Vector (Int, Int))

instance VM.MVector VU.MVector Range where
  basicLength (MV_Range v) = VM.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i i' (MV_Range v) = coerce $ VM.basicUnsafeSlice i i' v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Range v) (MV_Range v') = VM.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew i = MV_Range <$> VM.basicUnsafeNew i
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Range v) = VM.basicInitialize v
  {-# INLINE basicInitialize #-}
  basicUnsafeRead (MV_Range v) i = uncurry Range <$> VM.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Range v) i x = VM.basicUnsafeWrite v i (x ^. tuple)
  {-# INLINE basicUnsafeWrite #-}

instance V.Vector VU.Vector Range where
  basicUnsafeFreeze (MV_Range v) = V_Range <$> V.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Range v) = MV_Range <$> V.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Range v) = V.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i i' (V_Range v) = V_Range $ V.basicUnsafeSlice i i' v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Range v) i = view (from tuple) <$> V.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}

{-# COMPLETE RangeV #-}

pattern RangeV :: Int -> Int -> Range
pattern RangeV start end <- Range {start, end}

tuple :: Iso' Range (Int, Int)
tuple = iso (\Range {start, end} -> (start, end)) (\(start, end) -> Range {start, end})

_start :: Getter Range Int
_start = to start

_end :: Getter Range Int
_end = to end

unsafeNew :: Int -> Int -> Range
unsafeNew = Range

new :: Int -> Int -> Range
new start end =
  if start > end
    then error "start cannot be greater than end"
    else Range {start, end}

length :: Range -> Int
length Range {start, end} = end - start