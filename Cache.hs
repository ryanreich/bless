module Cache (makeCache) where

import Data.Bits (xor)
import Data.ByteString (ByteString, head, zipWith, pack)
import Data.List (genericReplicate)

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
      let zero = pack $ genericReplicate (bytesInItem PoW) 0
      in  iterate (hashS PoW) zero !!! (epochs when + 1)

eRMH :: Integer -> CacheB -> CacheB
eRMH when cache0 = cache
  where
    cache = asCache when $ List.zipWith3 eRMHit' [0..] cacheLPrev cacheL
    cacheL = elems cache0
    cacheLPrev = RAM.last cache0 : cacheL
    eRMHit' = eRMHit . getter
    getter n i
      | n < 255 && i >= n = cache0 ! i
      | otherwise = cache ! i

eRMHit :: (Integer -> ByteString) -> ByteString -> ByteString -> ByteString
eRMHit getter prev curr =
  hashS Cache $ pack $ zipWith xor prev item
  where item = getter (fromIntegral $ head curr)
