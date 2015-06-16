module Dataset.Mix (makeDatasetItemMix) where

import Dataset.Types
import Dataset.Round
import Types

import Data.List (genericReplicate)
import Prelude hiding (round)

makeDatasetItemMix :: DSMixM ()
makeDatasetItemMix = perform $ sequence_ $ genericReplicate (rounds Dataset) $
                     makeDatasetItemMixRound >>= storeMix
