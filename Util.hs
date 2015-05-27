module Util where

import Data.Binary as Binary (Binary, encode, decode)
import Data.Bits (FiniteBits(..), xor)
import Data.ByteString.Lazy (toStrict, fromStrict, concat, empty, append, take)
import qualified Data.ByteString as Strict (ByteString, unpack)
import Data.List (genericIndex)

import Prelude hiding (concat, take)

import RAM
import Types

(!!!) :: (Integral i) => [a] -> i -> a
(!!!) = genericIndex

encodeS :: (Binary a) => a -> Strict.ByteString
encodeS = toStrict . Binary.encode

decodeS :: (Binary a) => Strict.ByteString -> a
decodeS = Binary.decode . fromStrict

fnv :: EthWord -> EthWord -> EthWord
fnv x y =
  let fnvPrime = 0x01000193
  in x * (fnvPrime `xor` y)

fst4 :: (a,b,c,d) -> a
fst4 (x,_,_,_) = x

odds :: [a] -> [a]
odds (w0:w1:ws) = w1:odds ws
odds _ = []

asGroupsOf :: Int -> [a] -> [[a]]
asGroupsOf n x = xh : asGroupsOf n xt
  where (xh, xt) = splitAt n x

asWords :: Strict.ByteString -> [EthWord]
asWords = map asNum . asGroupsOf jWordBytes . Strict.unpack
  where jWordBytes = 4 -- EthWord = Word32

firstWord :: (Binary a) => a -> EthWord
firstWord = head . asWords . encodeS

asNum :: (Binary a, FiniteBits a, Binary b) => [a] -> b
asNum = decode . concat . map padEncode
  where
    padEncode x = padTo (finiteBitSize x `quot` 8) $ encode x
    padTo n bs = take (fromIntegral n) $ append bs empty

replaceAtBy :: RAM a -> Integer -> a -> RAM a
replaceAtBy ram i v = ram { values = f }
  where f j
          | i == j     = v
          | otherwise = values ram j

