module Cache where

import Control.Loop
import Control.Monad
import Control.Monad.ST

import Data.Bits (xor)

import qualified Data.Vector as V hiding (Vector, MVector)
import Data.Vector (Vector, MVector)
import qualified Data.Vector.Mutable as MV hiding (STVector)
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Storable as UV
import qualified Data.Vector.Storable.Mutable as UMV
import qualified Data.Vector.Generic as VG

import Utils

type CacheItem = UV.Vector EthWord
type Cache = Vector CacheItem

makeCache :: Int -> CacheItem -> Cache
makeCache epoch seed = runST $ do
  cache <- V.unsafeThaw $ V.unsafeTail $
           V.iterateN (1 + cacheItems epoch) (hash cacheItemBytes) seed
  let cacheRound = forLoop 0 (< MV.length cache) (+1) $ \i -> do
        prev <- MV.unsafeRead cache $ if i == 0 then MV.length cache - 1 else i - 1
        curr <- MV.unsafeRead cache i
        let hCurr = fromIntegral $ UV.unsafeHead curr
        href <- MV.unsafeRead cache $ hCurr `rem` MV.length cache
        MV.unsafeWrite cache i $ hash cacheItemBytes $ UV.zipWith xor prev href
  replicateM_ cacheRounds cacheRound
  V.unsafeFreeze cache  

nextCacheSeed :: CacheItem -> CacheItem
nextCacheSeed seed = hash cacheSeedBytes seed

initCacheSeed :: CacheItem
initCacheSeed = UV.replicate cacheSeedBytes 0

cacheSeedBytes :: Int
cacheSeedBytes = 32

cacheItemBytes :: Int
cacheItemBytes = 64

cacheRounds :: Int
cacheRounds = 3

cacheItems :: Int -> Int
cacheItems = epochGrowth (2^24) (2^17) cacheItemBytes

cacheItemWords :: Int
cacheItemWords = fromIntegral $ cacheItemBytes `quot` 4
