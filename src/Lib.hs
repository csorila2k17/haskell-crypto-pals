{-# LANGUAGE OverloadedStrings #-}

module Lib (hexToBase64, xor) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Base64.Lazy as B64 (encode)
import qualified Data.ByteString.Base16.Lazy as B16 (encode, decode)
import qualified Data.Bits as Bits (xor)

-- | Converts a base 16 string to a base 64 string
hexToBase64 :: BS.ByteString -> BS.ByteString
hexToBase64 = B64.encode . fst . B16.decode

-- | Returns a byte-wise xor of the two given strings.
xor :: BS.ByteString -> BS.ByteString -> BS.ByteString
xor a b = B16.encode $ BS.pack $ BS.zipWith Bits.xor (head bs) (last bs)
  where bs = fmap (fst . B16.decode) [a, b]
