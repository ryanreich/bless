{-# LANGUAGE RankNTypes #-}

module Utils where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Functor
import Data.Monoid

import Data.Binary
import Data.Binary.Get
import Data.ByteString.Builder
import Data.ByteString (ByteString)

import Data.ByteString.Unsafe
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Vector (Vector)
import qualified Data.Vector.Generic as VG
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe

import qualified Crypto.Hash.SHA3 as SHA3
import Data.Bits (xor)
import Data.List.Split (chunksOf)
import Data.Word
import Math.NumberTheory.Primes.Testing (isPrime)

type EthWord = Word32

fnvPrime = 0x01000193

fnv :: EthWord -> EthWord -> EthWord
fnv x y = (x * fnvPrime) `xor` y

odds :: [a] -> [a]
odds (w0:w1:ws) = w1:odds ws
odds _ = []

epochGrowth :: (Integral a) => a -> a -> a -> a -> a
epochGrowth init growth item epoch =
  fromIntegral $ maxPrimeBelow epochGoal
  where
    epochGoal = fromIntegral $ (init + growth * epoch - item) `quot` item
    maxPrimeBelow n
      | isPrime n = n
      | otherwise = maxPrimeBelow (n - 2) -- n is odd here
      {-- | even n = maxPrimeBelow (n - 1) --}
                    
epochs :: Int -> Int
epochs when = when `quot` 30000

asGroupsOf :: Int -> [a] -> [[a]]
asGroupsOf = chunksOf

hash :: (VS.Storable a) => Int -> VS.Vector a -> VS.Vector a
hash bytes = bs2vec . SHA3.hash (8 * bytes) . vec2bs

bs2vec :: (VS.Storable a) => ByteString -> VS.Vector a
bs2vec bs =
  let castToVector (cstring, len) = do
        fptr <- newForeignPtr_ $ castPtr cstring
        return $ VS.unsafeFromForeignPtr0 fptr len
  in unsafeDupablePerformIO $ unsafeUseAsCStringLen bs castToVector

vec2bs :: (VS.Storable a) => VS.Vector a -> ByteString
vec2bs vec =
  let (fptr, len) = VS.unsafeToForeignPtr0 vec
      castToString ptr = unsafePackCStringLen (castPtr ptr, len)        
  in unsafeDupablePerformIO $ withForeignPtr fptr castToString
