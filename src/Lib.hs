{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (
  toBase16,
  fromBase16,
  hexToBase64,
  perms,
  similarity,
  xor,
  xorKeys,
  histogram,
  Histogram(..),
  frequencies,
  kernel,
  FrequencyTable(..),
  english,
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM, (>>=), (<=<))
import Data.Char (ord)
import Data.Csv
import Data.List
import Data.Ord (compare)
import Data.String (IsString)
import Data.Word
import Linear
import qualified Data.Bits as Bits (xor)
import qualified Data.ByteString.Base16.Lazy as B16 (encode, decode)
import qualified Data.ByteString.Base64.Lazy as B64 (encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

-- | Base64 is a base64 encoded byte string
newtype Base64 = Base64 BS.ByteString
  deriving (IsString, Eq, Show)

-- | Base16 is a base16 (hex) encoded byte string
newtype Base16 = Base16 BS.ByteString
  deriving (IsString, Eq, Show)

-- | Histogram is a paramterized map of a given type to Integer counts.
newtype Histogram k = Histogram (Map.Map k Integer)
  deriving (Eq, Show)

-- | FrequencyTable maps k to the percentage it represents in a given alphabet.
newtype FrequencyTable k = FrequencyTable { fromFreqs :: Map.Map k Double }
  deriving (Eq, Ord, Show)

type Ngram = BS.ByteString

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

-- | Returns a histogram of counts of each distinct ngram of length n present
--   in the given string.
histogram :: Int -> BS.ByteString -> Histogram Ngram
histogram 0 _ = Histogram Map.empty
histogram n s = Histogram $ Map.fromListWith (+) pairs where
  pairs = zip (ngrams' n s) $ repeat 1
  ngrams' n ss
    | (fromIntegral . BS.length) ss < n = []
    | otherwise = BS.take (fromIntegral n) ss : ngrams' n (BS.drop 1 ss)

-- | Returns a FrequencyTable from the given Histogram.
frequencies :: Histogram Ngram -> FrequencyTable Ngram
frequencies (Histogram h) = FrequencyTable $ fmap (truncate 5 . freq) h where
  freq x = fromIntegral x / fromIntegral (sum h)
  truncate n f = fromInteger (round $ f * (10^n)) / (10.0^^n)

-- | Returns the xor keys of limit size sorted by the similarity of the
--   frequency table of the produced plaintext with the given reference.
xorKeys :: FrequencyTable Ngram -> Int -> BS.ByteString -> [BS.ByteString]
xorKeys _   0     _      = []
xorKeys ref limit cipher = sortBy similarity' keys where
  similarity' a b = similarity ref (freqs b) `compare` similarity ref (freqs a)
  freqs k = frequencies $ histogram 1 $ xor cipher k
  keys = perms limit [0..maxBound::Word8]

-- | Returns all possible byte strings of the given length with the given
--   alphabet.
perms :: Int -> [Word8] -> [BS.ByteString]
perms limit alphabet = BS.pack <$> replicateM limit (sort alphabet)

-- | Returns the similarity score of two frequency tables ranging from 0 to 1,
--   where 0 means completely different and 1 means identical.
similarity :: FrequencyTable Ngram -> FrequencyTable Ngram -> Double
similarity (FrequencyTable a) (FrequencyTable b) = dot a b / (norm a * norm b)

-- | English is the English language Ngram frequency table.
english :: FrequencyTable Ngram
english = FrequencyTable $ Map.fromList $ zip chars freqs where
  chars = C.singleton <$> ['A' .. 'Z']
  freqs = cycle [
    0.0804, 0.0148, 0.0334, 0.0382, 0.1249, 0.0240, 0.0187, 0.0505,
    0.0757, 0.0016, 0.0054, 0.0407, 0.0251, 0.0723, 0.0764, 0.0214,
    0.0012, 0.0628, 0.0651, 0.0928, 0.0273, 0.0105, 0.0168, 0.0023,
    0.0166, 0.0009]

-- | Returns a kernel string for the given frequency table
kernel :: FrequencyTable Ngram -> BS.ByteString
kernel (FrequencyTable f) = BS.concat $ Map.foldrWithKey cons [] f where
  cons ngram freq xs = xs ++ replicate (round $ freq * 10000) ngram

-- | Loads the frequency table from the given file.
{-readFrequencies :: String -> IO (Either String (Histogram Ngram))-}
{-readFrequencies filename = do-}
  {-contents <- BS.readFile filename-}
  {-case decode HasHeader contents of-}
    {-Left err -> return err-}
    {-Right v -> V.forM_ v $ \(ngram, count :: Integer) ->-}
      {-print $ ngram ++ ": " ++ count-}

  {-return Right (Histogram Map.empty)-}
