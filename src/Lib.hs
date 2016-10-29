{-# LANGUAGE OverloadedStrings #-}

module Lib (hexToBase64, xor) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Base64.Lazy as B64 (encode)
import qualified Data.ByteString.Base16.Lazy as B16 (encode, decode)
import qualified Data.Bits as Bits (xor)

hexToBase64 :: BS.ByteString -> BS.ByteString
hexToBase64 = B64.encode . fst . B16.decode

xor :: BS.ByteString -> BS.ByteString -> BS.ByteString
xor p k = B16.encode $ BS.pack $ BS.zipWith Bits.xor (head bs) (last bs)
  where bs = fmap (fst . B16.decode) [p, k]
