module Utils
  ( scanl'',
  )
where
  
import MyPrelude

scanl'' :: (b -> a -> b) -> b -> [a] -> [b]
scanl'' f z = drop 1 . scanl' f z