module Types (
  EthWord, CacheWord, CacheB, CacheMinor, Cache, Dataset, PoWStage(..),
  rounds, epochs,
  bytesInItem, wordsInItem,
  roomItems, roomsAt, itemsAt,
  hash, hashS
  ) where

import Blockchain.ExtWord (Word512)
import qualified Crypto.Hash.SHA3 as SHA3
import Data.ByteString.Lazy (ByteString, fromStrict)
import qualified Data.ByteString as Strict
import Data.Word
import Math.NumberTheory.Primes.Testing (isPrime)

import RAM

type EthWord = Word32 -- jWordBytes = 4
type CacheWord = Word512 -- bytesInItem Cache = 64 -> 512 bits

type CacheB = RAM Strict.ByteString
type CacheMinor = RAM Strict.ByteString
type Cache = RAM [EthWord]
type Dataset = RAM [EthWord]

data PoWStage = Cache | Dataset | Mix | PoW

rounds :: PoWStage -> Integer
rounds stage = case stage of
  Cache   ->
    let jCacheRounds = 3
    in jCacheRounds
  Dataset ->
    let jParents = 2^8
    in jParents
  Mix     ->
    let jAccesses = 2^6
    in jAccesses
  PoW     -> 2^64

bytesInItem :: PoWStage -> Integer
bytesInItem stage = case stage of
  Cache ->
    let jHashBytes = 64
    in jHashBytes
  Dataset -> bytesInItem Cache
  Mix ->
    let jMixBytes = 128
    in jMixBytes
  PoW -> 32

wordsInItem :: PoWStage -> Integer
wordsInItem stage =
  let jWordBytes = 4
  in bytesInItem stage `quot` jWordBytes

roomsInitial :: PoWStage -> Integer
roomsInitial stage = case stage of
  Cache   ->
    let jCacheInit = 2^24
    in jCacheInit `quot` bytesInItem Cache
  Dataset ->
    let jDatasetInit = 2^30
    in jDatasetInit `quot` bytesInItem Mix
  Mix     ->
    let jMixBytes = 2^7
    in jMixBytes `quot` bytesInItem Dataset
  PoW     -> 1

roomsEpoch :: PoWStage -> Integer
roomsEpoch stage = case stage of
  Cache   ->
    let jCacheGrowth = 2^17
    in jCacheGrowth `quot` bytesInItem Cache
  Dataset ->
    let jDatasetGrowth = 2^23
    in jDatasetGrowth `quot` bytesInItem Dataset
  Mix     -> 0
  PoW     -> 0
  
roomItems :: PoWStage -> Int
roomItems stage = case stage of
  Cache   -> 1
  Dataset -> 2 -- bytesInItem Mix `div` bytesInItem Dataset
  Mix     -> 2
  PoW     -> 8
  
roomsAt :: PoWStage -> Integer -> Integer
roomsAt which when = maxPrimeBelow epochGoal
  where
    epochGoal =
      roomsInitial which + roomsEpoch which * epochs when - 1
    maxPrimeBelow n
      | isPrime n = n
      | otherwise = maxPrimeBelow (n - 2) -- n is odd here
      {-- | even n = maxPrimeBelow (n - 1) --}

itemsAt :: PoWStage -> Integer -> Integer
itemsAt which when = (fromIntegral $ roomItems which) * (roomsAt which when)

epochs :: Integer -> Integer
epochs when = when `quot` jEpochBlocks
  where jEpochBlocks = 30000

hash :: PoWStage -> ByteString -> ByteString
hash Mix = fromStrict . SHA3.hashlazy 256
hash _   = fromStrict . SHA3.hashlazy 512

hashS :: PoWStage -> Strict.ByteString -> Strict.ByteString
hashS Mix = SHA3.hash 256
hashS _   = SHA3.hash 512

