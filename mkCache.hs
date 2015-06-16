module Main where

import Cache
import Data.Binary (encodeFile)

main = encodeFile "cache" $ makeCache 0

