--import PoW
import Dataset.Types
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

main = do
  cache <- decodeFile "cache"
  print $ invoke makeDataset cache 0 !0
  --encodeFile "dataset" $ makeDataset cache 0
