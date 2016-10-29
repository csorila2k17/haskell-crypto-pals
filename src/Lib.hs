{-# LANGUAGE OverloadedStrings #-}

module Lib (hexToBase64, xor, histogram) where

import qualified Data.ByteString as BS
import Data.Word
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.Bits as Bits (xor)
import qualified Data.Map.Strict as Map

-- | Converts a base 16 string to a base 64 string
hexToBase64 :: BS.ByteString -> BS.ByteString
hexToBase64 = B64.encode . fst . B16.decode

-- | Returns a byte-wise xor of the two given strings.
xor :: BS.ByteString -> BS.ByteString -> BS.ByteString
xor a b = B16.encode $ BS.pack $ BS.zipWith Bits.xor a' b'
  where [a', b'] = fmap (fst . B16.decode) [a, b]

-- | Returns a histogram of counts of each distinct byte present in the given
--   string.
histogram :: BS.ByteString -> Map.Map Word8 Integer
histogram = BS.foldl count Map.empty
  where count h b = Map.insertWith (+) b 1 h
