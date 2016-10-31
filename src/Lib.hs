{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib (
  toBase16,
  fromBase16,
  hexToBase64,
  perms,
  similarity,
  xor,
  xorKey,
  histogram,
  Histogram(..),
  frequencies,
  distances,
  kernel,
  FrequencyTable(..),
  english,
) where

import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import Data.List
import Data.Ratio (Rational(..), (%))
import Data.Char (ord)
import Data.Ord (compare)
import Data.String (IsString)
import Data.Word
import qualified Data.Bits as Bits (xor)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Base16.Lazy as B16 (encode, decode)
import qualified Data.ByteString.Base64.Lazy as B64 (encode)
import qualified Data.Map.Strict as Map

-- | Base64 is a base64 encoded byte string
newtype Base64 = Base64 BS.ByteString
  deriving (IsString, Eq, Show)

-- | Base16 is a base16 (hex) encoded byte string
newtype Base16 = Base16 BS.ByteString
  deriving (IsString, Eq, Show)

-- | Histogram is a paramterized map of a given type to Int counts.
newtype Histogram k = Histogram (Map.Map k Integer)
  deriving (Eq, Show)

-- | FrequencyTable maps k to the percentage it represents in a given alphabet.
newtype FrequencyTable k = FrequencyTable { fromFreqs :: Map.Map k Rational }
  deriving (Eq, Ord, Show)

-- | Returns a decoded BS.ByteString from a Base16 encoded string
fromBase16 :: Base16 -> BS.ByteString
fromBase16 (Base16 s) = fst $ B16.decode s

-- | Returns a decoded BS.ByteString to a Base16 encoded string
toBase16 :: BS.ByteString -> Base16
toBase16 = Base16 . B16.encode

-- | Returns an encoded Base64 string from a ByteString
toBase64 :: BS.ByteString -> Base64
toBase64 = Base64 . B64.encode

-- | Converts a base16 string to a base64 string
hexToBase64 :: Base16 -> Base64
hexToBase64 = toBase64 . fromBase16

-- | Returns a byte-wise encoded xor byte string of the two given byte strings.
--   The smaller string will be cycled to match the length of the longest.
xor :: BS.ByteString -> BS.ByteString -> BS.ByteString
xor a b = BS.pack $ BS.zipWith Bits.xor a' b' where
  [a', b'] = BS.take (maximum $ fmap BS.length [a, b]) . BS.cycle <$> [a, b]

-- | Returns a histogram of counts of each distinct byte present in the given
--   string.
histogram :: BS.ByteString -> Histogram Word8
histogram = Histogram . BS.foldr count Map.empty where
  count e = Map.insertWith (+) e 1

-- | Returns a FrequencyTable from the given Histogram.
frequencies :: Histogram Word8 -> FrequencyTable Word8
frequencies (Histogram h) = FrequencyTable $ fmap freq h where
  freq x = fromIntegral x / fromIntegral (sum h)

-- | Returns the xor key of at most limit size that produces the most similar
--   frequency table to the given reference when xored with the cipher.
xorKey :: FrequencyTable Word8 -> Int -> BS.ByteString -> BS.ByteString
xorKey _   0     _      = ""
xorKey ref limit cipher = maximumBy similarity' keys where
  similarity' a b = similarity ref (freqs a) `compare` similarity ref (freqs b)
  freqs k = frequencies $ histogram $ xor cipher k
  keys = perms limit [0..maxBound::Word8]

-- | Returns all possible byte strings of the given length with the given
--   alphabet.
perms :: Int -> [Word8] -> [BS.ByteString]
perms limit alphabet = BS.pack <$> replicateM limit (sort alphabet)

-- | Returns the similarity score of two frequency tables ranging from 0 to 1,
--   where 0 means completely different and 1 means identical.
similarity :: FrequencyTable Word8 -> FrequencyTable Word8 -> Rational
similarity a b | total == 0 = 0 | otherwise = 1 - (sum dists / total) where
  total = fromIntegral $ Map.size dists
  dists = distances a b

-- | Computes a map of element-wise distances between frequency tables.
distances :: FrequencyTable Word8 -> FrequencyTable Word8 -> Map.Map Word8 Rational
distances (FrequencyTable a) (FrequencyTable b) = Map.intersectionWith distf a b
  where distf x y = abs (x - y)

-- | English is the English language frequency table.
english :: FrequencyTable Word8
english = FrequencyTable $ Map.fromList $ zip chars freqs where
  chars = fromIntegral . ord <$> ['A' .. 'Z'] ++ ['a' .. 'z']
  freqs = cycle [
    0.0804, 0.0148, 0.0334, 0.0382, 0.1249, 0.0240, 0.0187, 0.0505,
    0.0757, 0.0016, 0.0054, 0.0407, 0.0251, 0.0723, 0.0764, 0.0214,
    0.0012, 0.0628, 0.0651, 0.0928, 0.0273, 0.0105, 0.0168, 0.0023,
    0.0166, 0.0009]

-- | Returns a kernel string for the given frequency table
kernel :: FrequencyTable Word8 -> BS.ByteString
kernel (FrequencyTable f) = BS.pack $ Map.foldrWithKey cons [] f where
  cons x n xs = xs ++ replicate (ceiling $ fromRational n * 100) x
