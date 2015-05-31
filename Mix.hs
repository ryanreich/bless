module Mix (onePoW) where

import Data.Binary (encode)
import Data.Bits (xor)
import Data.ByteString.Lazy (ByteString(..), append)

import RAM
import Types
import Util

import Debug.Trace

onePoW :: Dataset -> ByteString -> (EthWord, ByteString)
onePoW dataset headnonce =
  (cmix, append seed (encode cmix))
  where
    cmix = eCompress $ eAccesses dataset (unpackWords seed)
    seed = hash Mix headnonce

eCompress :: [EthWord] -> EthWord
eCompress mix = foldl1 fnv $ concat $ odds $ asGroupsOf 4 mix

eAccesses :: Dataset -> [EthWord] -> [EthWord]
eAccesses dataset seed =
  fst4 $ iterate eMixDataset (seedRep, dataset, head seed, 0) !!! rounds Mix
  where
    seedRep = concat $ replicate (roomItems Mix) seed

type MixTuple = ([EthWord], Dataset, EthWord, Integer)
eMixDataset :: MixTuple -> MixTuple
eMixDataset (mix, dataset, word, i) =
  (zipWith fnv mix (eNewData mix dataset word i), dataset, word, i + 1)

eNewData :: [EthWord] -> Dataset -> EthWord -> Integer -> [EthWord]
eNewData mix dataset word i =
  dataset ! (fromIntegral $ fnv (fromIntegral i `xor` word) (mix !!! i))
