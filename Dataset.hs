module Dataset (makeDataset) where

import Data.Bits (xor)
  
import RAM
import Types
import Util

makeDataset :: Cache -> Integer -> Dataset
makeDataset cache when = asDataset when rooms
  where rooms =
          let r = fromIntegral $ roomItems Dataset
          in map (makeDatasetRoom cache) [0,r..]

makeDatasetRoom :: Cache -> Integer -> [EthWord]
makeDatasetRoom cache i =
  concat $ map (makeDatasetItem cache) $ take (roomItems Dataset) [i..]      

makeDatasetItem :: Cache -> Integer -> [EthWord]
makeDatasetItem cache i =
  let cacheItem = cache ! i
      modifyCache0 = (head cacheItem `xor` (fromIntegral i)) : tail cacheItem
      mix0 = unpackWords $ hash Dataset $ packWords modifyCache0
      (mix1, _, _) = iterate (eMix cache i) (mix0, mix0, 0) !!! rounds Dataset
  in unpackWords $ hash Dataset $ packWords mix1

type DatasetPair = ([EthWord], [EthWord], Integer)
eMix :: Cache -> Integer -> (DatasetPair -> DatasetPair)
eMix cache i (mix, mixTail, p) =
  (newMix, newMixTail, p + 1)
  where
    newMix = zipWith fnv mix (cache ! j)
    j = fromIntegral $ fnv (fromIntegral $ i `xor` p) (head mixTail)
    newMixTail = if null t then newMix else t
      where t = tail mixTail
