import Data.ByteString

import PoW
import Dataset
import Cache
import Util
import Types

main = print $ minePoW header dataset difficulty
  where
    difficulty = 131072
    header = hash PoW $ pack [1,2,3,4]
    dataset = makeDataset cache 0
    cache = makeCache 0
