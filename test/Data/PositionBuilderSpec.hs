
module Data.PositionBuilderSpec (spec) where

import MyPrelude
import Data.PositionBuilder (build, (#))
import Test.Hspec

spec :: Spec
spec = do
  it "smoke" $ do
    -- print $ build ((("hello" # 1) # "world") # 1 # "banother")
    -- print $ build @(Either _) (" " # 1)
    -- print $ build (1 # "" # 1)
    -- print $ build (1 # "" # 1 # "" # 1 # "")
    -- print $ build (1 # " " # 1)
    return @IO ()
  return ()
