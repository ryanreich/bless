--import PoW
import Dataset
import Cache
import Util
import Types
import RAM

import Data.Binary

-- main = print $ minePoW header dataset difficulty
--   where
--     difficulty = 131072
--     header = hash PoW $ pack [1,2,3,4]
--     dataset = makeDataset cache 0
--     cache = makeCache 0

writeCache = encodeFile "cache" $ makeCache 0

main = do
  cache <- decodeFile "cache"
  encodeFile "dataset" $ makeDataset cache 0
