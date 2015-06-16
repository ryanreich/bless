module Dataset.Round (makeDatasetItemMixRound) where

import Dataset.Types
import Types
import Util
import RAM

import Control.Monad
import Data.Bits

import Prelude hiding (round)

makeDatasetItemMixRound :: DSRoundM [EthWord]
makeDatasetItemMixRound = liftM2 (zipWith fnv) mix cacheItem

cacheItem :: DSRoundM [EthWord]
cacheItem = liftM2 (cycle .* (!)) cache mixFNV

mixFNV :: DSRoundM Integer
mixFNV = liftM2 (fromIntegral .* fnv) modifiedIndex mixWord

modifiedIndex :: DSRoundM EthWord
modifiedIndex = liftM2 (fromIntegral .* xor) round (step >> item)

mixWord :: DSRoundM EthWord
mixWord = liftM (!!! 1) mix
