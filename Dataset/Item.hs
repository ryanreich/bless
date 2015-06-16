module Dataset.Item (makeDatasetItem) where

import Dataset.Types
import Dataset.Mix
import Types
import Util
import RAM

import Control.Arrow
import Control.Monad
import Data.Bits
import Data.ByteString.Lazy
import Data.List (genericTake)

makeDatasetItem :: DSItemM ByteString
makeDatasetItem = liftM finalizeItem $ exec makeDatasetItemMix mix0
  where finalizeItem = hash Dataset . packWords . genericTake (wordsInItem Dataset)

mix0 :: DSItemM [EthWord]
mix0 = liftM (unpackWords . hash Dataset . packWords) modifiedCache

modifiedCache :: DSItemM [EthWord]
modifiedCache = step >> (liftM rejoin $ xorFirst $ liftM break $ cacheAtItem)
  where break (h:t) = (h,t)
        rejoin = uncurry (:)

cacheAtItem :: DSItemM [EthWord]
cacheAtItem = liftM2 (!) cache item

xorFirst :: DSItemM (EthWord, [EthWord]) -> DSItemM (EthWord, [EthWord])
xorFirst = liftM2 (first . xor . fromIntegral) item

