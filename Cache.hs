module Cache (makeCache) where

import Data.Bits (xor)
import Data.ByteString (ByteString, head, zipWith, pack)
import Data.List (genericTake)

import Prelude hiding (head, zipWith)
import qualified Data.List as List (head, zipWith, zipWith3)
  
import RAM
import Types
import Util

makeCache :: Integer -> Cache
makeCache when =
  unpackCacheB $ iterate (eRMH when) (cacheInit when) !!! rounds Cache

cacheInit :: Integer -> CacheB
cacheInit when = asCache when $ tail $ iterate (hashS Cache) init
  where
    init =
      let zero = pack $ genericTake (bytesInItem PoW) [0,0..]
      in  iterate (hashS PoW) zero !!! (epochs when + 1)

eRMH :: Integer -> CacheB -> CacheB
eRMH when cache0 = asCache when cache
  where
    cache = minorL ++ List.zipWith (eRMHit (minorR !)) cacheLtPrev cacheLt
    minorR = eRMHminor (last cacheL : cacheLh)
    minorL = elems minorR
    cacheLtPrev = last (minorL) : cacheLt
    (cacheLh, cacheLt) = splitAt (2^8) cacheL
    cacheL = elems cache0

eRMHminor :: [ByteString] -> RAM ByteString
eRMHminor cacheLprev = minor
  where
    minor = asCacheMinor $ List.zipWith3 eRMHit' [0..] cacheLprev cacheL
    eRMHit' = eRMHit . getter
    getter n i
      | i < n = minor ! i
      | otherwise = cacheR ! i
    cacheL = tail cacheLprev
    cacheR = asCacheMinor cacheL

eRMHit :: (Integer -> ByteString) -> ByteString -> ByteString -> ByteString
eRMHit getter prev curr =
  hashS Cache $ pack $ zipWith xor prev item
  where item = getter (fromIntegral $ head curr)
