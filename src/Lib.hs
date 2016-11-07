{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib (
  toBase16,
  fromBase16,
  hexToBase64,
  similarity,
  xor,
  keys,
  scores,
  keyScores,
  histogram,
  Histogram,
  frequencies,
  FrequencyTable,
  Ngram,
  ngrams,
  ngramFrequencies,
  decodeNgramsFromFile
) where

import Control.Applicative ((<$>))
import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.Char (ord, isAscii, isPrint, Char)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Data.Text.Lazy.Encoding (decodeUtf8', encodeUtf8)
import GHC.Generics (Generic)
import Linear
import Data.Csv
  ( FromRecord(..)
  , HasHeader(..)
  , decode
  )

import qualified Data.Text.Lazy as T
import qualified Data.Bits as Bits (xor)
import qualified Data.ByteString.Base16.Lazy as B16 (encode, decode)
import qualified Data.ByteString.Base64.Lazy as B64 (encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

-- | Base64 is a base64 encoded byte string
type Base64 = BS.ByteString

-- | Base16 is a base16 (hex) encoded byte string
type Base16 = BS.ByteString

-- | Key represents a secret key
type Key = BS.ByteString

-- | CipherText represents encrypted plain text
type CipherText = BS.ByteString

-- | PlainText represents decrypted cypher text
type PlainText = BS.ByteString

-- | Score represents a Ngram frequency similarity score
type Score = Double

-- | Ngram is an Ngram and its number of occurences in a corpus.
data Ngram = Ngram BS.ByteString Integer deriving (Show, Eq, Ord, Generic)

-- | We make Ngram an instance of CSV.FromNamedRecord in order to load
-- the Ngram frequency database that is encoded in this format.
instance FromRecord Ngram

-- | Histogram is a map of Ngrams to their counts.
type Histogram = Map.Map BS.ByteString Integer

-- | FrequencyTable is a map of Ngrams to their relative frequency in a corpus.
type FrequencyTable = Map.Map BS.ByteString Score

-- | Returns a decoded BS.ByteString from a Base16 encoded string
fromBase16 :: Base16 -> BS.ByteString
fromBase16 = fst . B16.decode

-- | Returns a decoded BS.ByteString to a Base16 encoded string
toBase16 :: BS.ByteString -> Base16
toBase16 = B16.encode

-- | Returns an encoded Base64 string from a ByteString
toBase64 :: BS.ByteString -> Base64
toBase64 = B64.encode

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
histogram :: [Ngram] -> Histogram
histogram ns = Map.fromListWith (+) (fmap pairs ns)
  where pairs (Ngram n c) = (n, c)

-- | Returns a list of Ngrams of size n from the given corpus.
ngrams :: BS.ByteString -> Int -> [Ngram]
ngrams _ 0 = []
ngrams s n
  | (fromIntegral . BS.length) s < n = []
  | otherwise = Ngram (BS.take (fromIntegral n) s) 1 : ngrams (BS.drop 1 s) n

-- | Returns a FrequencyTable from the given Histogram.
frequencies :: Histogram -> FrequencyTable
frequencies h = freq <$> h where freq x = fromIntegral x / fromIntegral (sum h)

-- | Returns a sorted list of tuples of single charachter xor keys and
-- similarity scores to the reference frequency table.
keyScores :: FrequencyTable -> CipherText -> [(Key, Score)]
keyScores ref cipher = sortBy cmp $ zip keys $ scores keys ref cipher where
  cmp  = comparing $ Down . snd

-- | Returns the set of ASCII keys of length 1 to be used for xor cracking.
keys :: [BS.ByteString]
keys = (BS.singleton . fromIntegral . ord) <$>
  filter isAsciiPrint [(minBound::Char)..(maxBound::Char)]

-- | Predicate on Chars that returns true for printable ASCII
isAsciiPrint :: Char -> Bool
isAsciiPrint c = isAscii c && isPrint c

-- | Returns a list of similarity scoresfor the given keys xored with the
-- given cipher text.
scores :: [Key] -> FrequencyTable -> CipherText -> [Score]
scores ks ref cipher = score <$> ks where
  score = similarity ref . frequencies . histogram . ngrams'
  ngrams' k = [lo..hi] >>= ngrams (decode' $ xor cipher k)
  decode' s = either (return "") normal' $ decodeUtf8' s
  normal' = encodeUtf8 . T.filter isAsciiPrint . T.toUpper
  [lo, hi] = (.) (fromIntegral . BS.length . fst) <$>
    [Map.findMin, Map.findMax] <*> [ref]

-- | Returns the similarity score of two frequency tables ranging from 0 to 1,
-- where 0 means completely different and 1 means identical. Implements the
-- cosine similarity formula.
similarity :: FrequencyTable -> FrequencyTable -> Score
similarity a b = dot a b / (norm a * norm b)

-- | Returns a FrequencyTable from the given Ngram vector.
ngramFrequencies :: V.Vector Ngram -> FrequencyTable
ngramFrequencies = frequencies . histogram . V.toList

-- | Reads and decodes Ngrams from a given file.
decodeNgramsFromFile :: FilePath -> IO (Either String (V.Vector Ngram))
decodeNgramsFromFile fp = try' (BS.readFile fp) >>= handle where
  handle = return . either Left (decode NoHeader)
  try' a = fmap Right a `catch` err
  err :: IOException -> IO (Either String a)
  err = return . Left . show
