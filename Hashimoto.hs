--module Hashimoto where
import System.Environment

import Data.Bits
import qualified Data.ByteString as ByteS
import qualified Crypto.Hash.SHA3 as SHA3

data Params =
  NBITS | NBYTES | BYTEBITS | BYTECAP |
  SAFEPRIME_512 | N | N_INC |
  CACHE_SIZE | DIFF | EPOCHTIME | ACCESSES | P

val param = case param of
  NBITS -> 512
  BYTEBITS -> 8
  BYTECAP -> 2 ^ val BYTEBITS
  NBYTES -> val NBITS `div` val BYTEBITS
  SAFEPRIME_512 -> 2^512 - 38117
  N -> 4000055296 * 8 `div` val NBITS
  N_INC -> 65536
  CACHE_SIZE -> 2500
  DIFF -> 2^14
  EPOCHTIME -> 100000
  ACCESSES -> 200
  P -> val SAFEPRIME_512

encode = ByteS.pack . toBigEndianByteSeq
  where toBigEndianByteSeq = map fromIntegral .  getBytes
        getBytes = (compose (val NBYTES - 1) attachQuotRem) . (:[])
        compose n f = (!! n) . iterate f
        attachQuotRem = mapHead (pairToList . (`quotRem` val BYTECAP))
        mapHead f (x : xs) = f x ++ xs
        pairToList (x,y) = [x,y]

decode :: ByteS.ByteString -> Integer
decode = fromBigEndianByteSeq . ByteS.unpack
  where fromBigEndianByteSeq = foldl f 0
        f s w = (val BYTECAP * s) + (fromIntegral w)

sha3 = decode . SHA3.hash 512 . encode

dblSha3 = decode . SHA3.hash 512 . SHA3.hash 512 . encode

modP = (`rem` val P)

twoPowPows :: Integer -> Int -> Integer
twoPowPows x 0 = x
twoPowPows x n = modP (twoPowPows x (n - 1))^2

powsOf :: Integer -> Int -> Integer
powsOf x 0 = 1
powsOf x n = modP $ repSquarePow n
  where
    repSquarePow n = (powsOf x n') * (twoPowPows x n0)
      where
        n0 = largestBitSet n
        n' = clearBit n n0
        largestBitSet m = maximum $ filter (testBit m) [0..finiteBitSize m]

dagPow = modP . cube
  where cube x = x * (modP x^2)

dag seed 0 = dagPow (sha3 seed)
dag seed n = f (powsOfInit $ n + 1) (powsOfInitMod $ n + 1)
  where f x y = dagPow $ x `xor` (dag seed $ fromIntegral y)
        powsOfInit = powsOf $ dag seed 0
        powsOfInitMod = modFunc powsOfInit
        modFunc f n = f n `rem` fromIntegral (n - 1)

main = do
  args <- getArgs
  print $ dag 0 (read $ head args)
