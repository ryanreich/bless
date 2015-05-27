module RAM (
  RAM(..), (!!)
  ) where

import Control.Monad
import Data.Binary
import Prelude hiding (length, (!!))

-- length is number of items, width is number of bytes in each item
data RAM a = RAM { values :: Integer -> a, length :: Integer, width :: Int }

(!!) :: RAM a -> Integer -> a
(!!) ram = values ram . (`mod` length ram)

-- instance (Binary a) => Binary (RAM a) where
--   put (RAM { values = f, length = n, width = w }) = do
--     put w
--     forM_ [0..n-1] (put . f)

--   get = do
--     w <- get
    
