module RAM (
  RAM, asRAM, (!), (//), len, Array.elems, Array.amap
  ) where

import qualified Data.Array.IArray as Array
import Data.List (genericTake)

type RAM = Array.Array Integer

asRAM :: Integer -> [a] -> RAM a
asRAM len ls = Array.listArray (0, len - 1) $ genericTake len ls

len :: RAM a -> Integer
len ram = 1 + snd (Array.bounds ram)

safeIndex :: RAM a -> Integer -> Integer
safeIndex ram i = (i + l) `rem` l
  where l = len ram

(!) :: RAM a -> Integer -> a
ram ! i = ram Array.! safeIndex ram i

(//) :: RAM a -> [(Integer, a)] -> RAM a
ram // [(i, e)] = ram Array.// [(safeIndex ram i, e)]
