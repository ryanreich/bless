module Dataset (makeDataset) where

import Data.Binary (encode, decode)
import Data.Bits (xor)

import Prelude hiding (length, (!!))
import qualified Data.List as List (genericLength)

import RAM
import Types
import Util

makeDataset :: Cache -> Integer -> Dataset
makeDataset cache when =
  RAM { values = makeDatasetRoom cache . ((*) $ fromIntegral $ roomItems Dataset),
        length = roomsAt Dataset when, width = 128 }

makeDatasetRoom :: Cache -> Integer -> [EthWord]
makeDatasetRoom cache i =
  concat $ map (makeDatasetItem cache) $ take (roomItems Dataset) [i..]      

makeDatasetItem :: Cache -> Integer -> [EthWord]
makeDatasetItem cache i =
  let bytes0 = encodeS $ (decodeS $ cache RAM.!! i) `xor` i
      mix0 = asWords $ hash Dataset bytes0
  in fst4 $ iterate eMix (mix0, cache, i, 0) !!! rounds Dataset
  where

type DatasetTuple = ([EthWord], Cache, Integer, Integer)
eMix :: DatasetTuple -> DatasetTuple
eMix (mix, cache, i, p) =
  (zipWith fnv mix $ asWords $ cache RAM.!! j, cache, i, p + 1)
  where j = fromIntegral $ fnv (firstWord i `xor` firstWord p) (mix !!! p')
        p' = p `rem` List.genericLength mix
