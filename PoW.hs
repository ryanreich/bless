module PoW where

import Data.Binary
import Data.ByteString.Lazy (ByteString, append, reverse)
import Data.List (find)
import Data.Maybe (fromMaybe)

import Prelude hiding (reverse)

import Mix
import Types
import Util

type Header = ByteString

minePoW :: Header -> Dataset -> Integer -> Integer
minePoW header dataset difficulty =
  fromMaybe (-1) $ find (<= target) (map hashi $ [0..rounds PoW - 1])
  where
    target = 2^256 `quot` difficulty
    num = rounds PoW
    hashi nonce =
      let headHash = hash PoW $ header
                     -- rlpSerialize, getResHead undefined
                     --fromStrict $ hash PoW $ rlpSerialize (getResHead header)
          nonceHash = reverse $ hash PoW $ encode nonce
      in decode $ hash PoW $ snd $ onePoW dataset $ append headHash nonceHash
