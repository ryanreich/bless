module Main where

import Dataset
import Utils

import Control.Monad
import Control.Monad.ST

import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Base16.Lazy as B16
import System.Environment

import qualified Data.Vector as V
import Data.Vector.Binary

import GHC.Profiling

main = do
  stopProfTimer
  cache <- decodeFile "./cache"
  args <- getArgs
  let when = if not (null args)
             then read $ head args
             else error "When required"
  startProfTimer
  let epoch = epochs when
      howmany = if not (null $ tail args)
                then read $ head $ tail args
                else datasetItems epoch
      dataset = makeDataset epoch cache howmany
  dataset `seq` return ()
  --encodeFile "./dataset" dataset
