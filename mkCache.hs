module Main where

import Cache
import Utils

import qualified Data.Vector as V
import Data.Vector.Binary

import Data.Bits
import System.Environment

import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Base16.Lazy as B16

import GHC.Profiling

main = do
  stopProfTimer
  args <- getArgs
  let when = if not (null args)
             then read $ head args
             else error "When required"
  startProfTimer
  let epoch = epochs when
      seed = iterate nextCacheSeed initCacheSeed !! epoch
      cache = makeCache epoch seed
  --cache `seq` return ()
  encodeFile "./cache" cache
