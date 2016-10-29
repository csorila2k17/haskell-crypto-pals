{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib (hexToBase64, xor, histogram, Histogram(..)) where

import Data.String (IsString)
import Data.Word
import qualified Data.Bits as Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Data.Map.Strict as Map

-- | Base64 is a base64 encoded byte string
newtype Base64 = Base64 { fromBase64 :: BS.ByteString }
  deriving (IsString, Eq, Show)

-- | Base16 is a base16 (hex) encoded byte string
newtype Base16 = Base16 { fromBase16 :: BS.ByteString }
  deriving (IsString, Eq, Show)

-- | Histogram is a paramterized map of given types of keys to Integer counts.
newtype Histogram k = Histogram (Map.Map k Integer)
  deriving (Eq, Show)

-- | Converts a base16 string to a base64 string
hexToBase64 :: Base16 -> Base64
hexToBase64 = Base64 . B64.encode . fst . B16.decode . fromBase16

-- | Returns a byte-wise base16 encoded xor of the two given base16 strings.
xor :: Base16 -> Base16 -> Base16
xor a b = Base16 . B16.encode . BS.pack $ BS.zipWith Bits.xor a' b'
  where [a', b'] = fmap (fst . B16.decode . fromBase16) [a, b]

-- | Returns a histogram of counts of each distinct byte present in the given
--   base16 encoded string.
histogram :: Base16 -> Histogram Word8
histogram = Histogram . BS.foldl count Map.empty . fromBase16
  where count h b = Map.insertWith (+) b 1 h
