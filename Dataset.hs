{-# LANGUAGE RankNTypes #-}

module Dataset where

import Cache
import Utils

import Control.Loop
import Control.Monad
import Control.Monad.ST
import Data.Functor
import Data.Monoid

import Data.Vector (Vector, MVector)
import qualified Data.Vector as V hiding (Vector, MVector)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as UV
import qualified Data.Vector.Storable.Mutable as UMV

import Data.Binary.Get
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import qualified Data.ByteString as B hiding (ByteString)
import qualified Data.ByteString.Lazy as BL hiding (fromStrict, toStrict)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Word

type DatasetItem = UV.Vector EthWord
type Dataset = Vector DatasetItem

makeDataset :: Int -> Cache -> Int -> Dataset
makeDataset when cache howmany =
  runST $ V.generateM howmany (makeDatasetItem cache)

makeDatasetItem :: Cache -> Int -> ST s DatasetItem
makeDatasetItem cache i = do
  item0 <- UV.thaw $ V.unsafeIndex cache $ i `rem` V.length cache
  UMV.unsafeModify item0 (`xor` fromIntegral i) 0
  fItem0 <- UV.unsafeFreeze item0
  item <- UV.unsafeThaw $ hash cacheItemBytes fItem0
  forLoop 0 (< datasetRounds) (+1) $ \j -> do
    itemj <- UMV.unsafeRead item $ j `rem` datasetItemWords
    let index = fnv (fromIntegral $ xor i j) itemj
    let cItem = V.unsafeIndex cache $ fromIntegral index `rem` cacheItemWords

    -- Why is this so much faster than forLoop???
    let doLoop k
          | k == datasetItemWords = return ()
          | otherwise = do
            let cItemk = UV.unsafeIndex cItem k
            UMV.unsafeModify item (`fnv` cItemk) k
            doLoop (k + 1)
    doLoop 0
  hash datasetItemBytes <$> UV.unsafeFreeze item

datasetItemBytes :: Int
datasetItemBytes = cacheItemBytes

datasetItemWords :: Int
datasetItemWords = datasetItemBytes `quot` 4

datasetRounds :: Int
datasetRounds = 256

mixDatasetItems :: Int
mixDatasetItems = 2

datasetMixes :: Int -> Int
datasetMixes = epochGrowth (2^30) (2^23) (datasetItemBytes * mixDatasetItems)

datasetItems :: Int -> Int
datasetItems = (mixDatasetItems *) . datasetMixes
