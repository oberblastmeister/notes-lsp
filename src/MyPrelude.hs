module MyPrelude
  ( module Relude,
    module Data.Generics.Labels,
    module Data.Default,
    show,
    print,
    traceShow,
    traceShowId,
    traceShowWith,
    traceShowM,
    MonadUnliftIO,
  )
where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Default
import Data.Generics.Labels
import qualified Debug.Trace as Debug
import Relude hiding (print, show, traceShow, traceShowId, traceShowM)
import Text.Pretty.Simple

class PrettyShow b where
  show :: Show a => a -> b

instance PrettyShow String where
  show = toString . toText . pShowNoColor
  {-# INLINE show #-}

instance PrettyShow Text where
  show = toText . pShowNoColor
  {-# INLINE show #-}

instance PrettyShow LText where
  show = pShowNoColor
  {-# INLINE show #-}

print :: (MonadIO m, Show a) => a -> m ()
print = pPrintNoColor
{-# INLINE print #-}

-- | Similar to 'trace' but prints a given value with the 'Show'
-- instance instead of a 'String'.
--
-- >>> increment l = map (+1) l
-- >>> increment [2, 3, 4]
-- [3,4,5]
--
-- >>> increment l = traceShow l (map (+1) l)
-- >>> increment [2, 3, 4]
-- [2,3,4]
-- [3,4,5]
--
-- * If you want to print a specific 'String' instead, use 'trace'
-- * If you want to print and return the same value, use 'traceShowId'
-- * If you want to specify a custom printing function, use 'traceShowWith'
traceShow :: Show a => a -> b -> b
traceShow a = Debug.trace (show a)
{-# WARNING traceShow "'traceShow' remains in code" #-}

-- | Similar to 'traceShow' but prints the given value itself instead
-- of a separate value.
--
-- >>> traceShowId (1+2+3, "hello" ++ "world")
-- (6,"helloworld")
-- (6,"helloworld")
--
-- * If you to specify a different value to print, use 'trace' or 'traceShow'
-- * If you want to have more control over printing, use 'traceShowWith'
traceShowId :: Show a => a -> a
traceShowId a = traceShow a a
{-# WARNING traceShowId "'traceShowId' remains in code" #-}

-- | Similar 'traceShowId', but uses a provided function to convert the
-- argument to a value with the 'Show' constraint.
--
-- >>> traceShowWith fst (1, "ABC")
-- 1
-- (1,"ABC")
--
-- In other words, @'traceShowId' ≡ 'traceShowWith' id@.
--
-- This function is useful for debugging values that do not have 'Show'
-- instance:
--
-- >>> fst $ traceShowWith fst (1, id)
-- 1
-- 1
--
-- * If you don't need such flexibility, use simpler 'trace', 'traceShow' or 'traceShowId'
--
-- @since 1.0.0.0
traceShowWith :: Show b => (a -> b) -> a -> a
traceShowWith f v = traceShow (f v) v
{-# WARNING traceShowWith "'traceShowWith remains in code" #-}

-- |
-- Like 'traceM', but uses 'Relude.show' on the argument to convert it to a
-- 'String'.
--
-- >>> :{
-- let action :: Maybe Int
--     action = do
--         x <- Just 3
--         traceShowM x
--         y <- pure 12
--         traceShowM y
--         pure (x*2 + y)
-- in action
-- :}
-- 3
-- 12
-- Just 18
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Debug.traceM . show
{-# WARNING traceShowM "'traceShowM' remains in code" #-}