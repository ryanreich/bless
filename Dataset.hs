module Dataset (makeDataset) where

import Dataset.Types
import Dataset.Room
import Types
import Util
import RAM

import Control.Monad hiding (when)

makeDataset :: DSCacheM Dataset
makeDataset = liftM2 asDataset when $ perform $ sequence $
              makeDatasetRoom : (repeat $ (advance $ roomItems Dataset) >> makeDatasetRoom)
