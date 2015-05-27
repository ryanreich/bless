module Types (
  EthWord, Cache, Dataset, PoWStage(..),
  rounds, roomItems, roomsAt, itemsAt, epochs, hash
  ) where

import qualified Crypto.Hash.SHA3 as SHA3
import Data.ByteString
import Data.Word
import Math.NumberTheory.Primes.Testing (isPrime)

import RAM

type EthWord = Word32 -- jWordBytes = 4
type Cache = RAM ByteString
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
hash Mix = SHA3.hash 256
hash _   = SHA3.hash 512
