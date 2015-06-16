module Dataset.Room (makeDatasetRoom) where

import Dataset.Item
import Dataset.Types
import Types
import Util

import Prelude hiding (concat)

import Control.Monad
import Data.ByteString.Lazy (concat)
import Data.List (genericReplicate)

makeDatasetRoom :: DSRoomM [EthWord]
makeDatasetRoom = from room $ liftM (unpackWords . concat) $ sequence $
                  genericReplicate (roomItems Dataset) makeDatasetItem
