module Cache (makeCache) where

import Data.Bits (xor)
import Data.ByteString (empty, head, zipWith, pack)

import Prelude hiding (length, head, zipWith, (!!))
  
import RAM
import Types
import Util

import Debug.Trace

makeCache :: Integer -> Cache
makeCache when = iterate eRMH (cacheInit when) !!! rounds Cache

cacheInit :: Integer -> Cache
cacheInit when = RAM { values = f, length = itemsAt Cache when, width = 64 }
  where
    f i
      | i == -1    = iterate (hash PoW) empty !!! epochs when
      | otherwise = hash Cache $ f (i - 1)

eRMH :: Cache -> Cache
eRMH cache = eRMHit 0 cache 

eRMHit :: Integer -> Cache -> Cache
eRMHit i cache
  | length cache == i = cache
  | otherwise =
    let item1 = cache RAM.!! (i - 1)
        item2 = cache RAM.!! (fromIntegral $ head $ cache RAM.!! i)
        val = hash Cache $ pack $ zipWith xor item1 item2
    in eRMHit (i + 1) $ replaceAtBy cache i val

