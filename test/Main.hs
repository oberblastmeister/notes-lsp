module Main (main) where
  
import MyPrelude
import qualified Spec
  
main :: IO ()
main = do
  Spec.main
  return ()