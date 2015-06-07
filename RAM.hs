module RAM (
  RAM, asRAM, (!), (//), len, Array.elems, Array.amap, RAM.last
  ) where

import qualified Data.Array.IArray as Array
import Data.List (genericTake)

type RAM = Array.Array Integer

asRAM :: Integer -> [a] -> RAM a
asRAM len = Array.listArray (0, len - 1) . genericTake len

lastIx :: RAM a -> Integer
lastIx = snd . Array.bounds

len :: RAM a -> Integer
len = (1 +) . lastIx

safeIndex :: RAM a -> Integer -> Integer
safeIndex ram i = rem (i + l) l
  where l = len ram

(!) :: RAM a -> Integer -> a
ram ! i = ram Array.! safeIndex ram i

(//) :: RAM a -> [(Integer, a)] -> RAM a
ram // [(i, e)] = ram Array.// [(safeIndex ram i, e)]

last :: RAM a -> a
last ram = ram ! lastIx ram
