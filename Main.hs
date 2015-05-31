--import PoW
import Dataset
import Cache
import Util
import Types
import RAM

import Network.Haskoin.Crypto (Word512)

import qualified Data.Array as Array
-- main = print $ minePoW header dataset difficulty
--   where
--     difficulty = 131072
--     header = hash PoW $ pack [1,2,3,4]
--     dataset = makeDataset cache 0
--     cache = makeCache 0

main = print $ elems $ makeDataset cache 0
  where cache = makeCache 0
