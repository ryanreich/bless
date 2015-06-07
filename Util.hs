module Util where

import Data.Binary as Binary (Binary, encode, decode)
import Data.Binary.Get
import Data.Bits (FiniteBits(..), xor)
import Data.Bool (bool)
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString, pack, unpack, concat, empty, length,
                             fromStrict)
import qualified Data.ByteString as Strict (ByteString)
import Data.Function (on)
import Data.Lens.Common
import Data.List (genericIndex)
import Data.List.Split (chunksOf)
import Data.Monoid

import Prelude hiding (concat, length)

import RAM
import Types

import Debug.Trace

asCache :: Integer -> [a] -> RAM a
asCache when = asRAM (itemsAt Cache when)

asDataset :: Integer -> [[EthWord]] -> Dataset
asDataset when = asRAM (roomsAt Dataset when)

(!!!) :: (Integral i) => [a] -> i -> a
(!!!) = genericIndex

fnv :: EthWord -> EthWord -> EthWord
fnv x y =
  let fnvPrime = 0x01000193
  in  (x * fnvPrime) `xor` y

odds :: [a] -> [a]
odds (w0:w1:ws) = w1:odds ws
odds _ = []

asGroupsOf :: Int -> [a] -> [[a]]
asGroupsOf = chunksOf

unpackWords :: ByteString -> [EthWord]
unpackWords bs = runGet (sequence $ replicate n getWord32be) bs
  where
    n = fromIntegral $ length bs `quot` 4
  
packWords :: [EthWord] -> ByteString
packWords = toLazyByteString . foldr (mappend . word32BE) (lazyByteString empty)

unpackCacheB :: CacheB -> Cache
unpackCacheB = amap (unpackWords . fromStrict)
