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
import Data.List (genericIndex, genericSplitAt)
import Data.List.Split (chunksOf)
import Data.Monoid

import Prelude hiding (concat, length)

import RAM
import Types

import Debug.Trace

asCache :: Integer -> [a] -> RAM a
asCache when = asRAM (itemsAt Cache when)

unpackCacheB :: CacheB -> Cache
unpackCacheB = amap (unpackWords . fromStrict)

asDataset :: Integer -> [[EthWord]] -> Dataset
asDataset when = asRAM (roomsAt Dataset when)

unpackWords :: ByteString -> [EthWord]
unpackWords bs = runGet (sequence $ replicate n getWord32be) bs
  where
    n = fromIntegral $ length bs `quot` 4
  
packWords :: [EthWord] -> ByteString
packWords = toLazyByteString . foldr (mappend . word32BE) (lazyByteString empty)

fnv :: EthWord -> EthWord -> EthWord
fnv x y =
  let fnvPrime = 0x01000193
  in  (x * fnvPrime) `xor` y

odds :: [a] -> [a]
odds (w0:w1:ws) = w1:odds ws
odds _ = []

asGroupsOf :: Int -> [a] -> [[a]]
asGroupsOf = chunksOf

shift :: [[a]] -> [[a]]
shift [] = []
shift ls = h : map tail (shift t)
  where ([h], t) = splitAt 1 ls

every :: (Integral a) => a -> (b -> b) -> [b] -> [b]
every _ _ [] = []
every n f ls = (f h : t) ++ (every n f rest)
  where (firstN, rest) = genericSplitAt n ls
        ([h], t) = splitAt 1 firstN

forceList :: [a] -> [a]
forceList [] = []
forceList [a] = [a]
forceList ls = h:t
  where t = forceList $ tail ls
        h = head t `seq` head ls

infixr 9 .*
(.*) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.*) = (.) . (.)

(!!!) :: (Integral i) => [a] -> i -> a
(!!!) = genericIndex
