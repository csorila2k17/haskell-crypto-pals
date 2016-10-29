{-# LANGUAGE OverloadedStrings #-}

module Lib (hexToBase64, xorFixed) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Base16.Lazy as B16
import Data.Bits (xor)

hexToBase64 :: B.ByteString -> B.ByteString
hexToBase64 = B64.encode . fst . B16.decode

xorFixed :: B.ByteString -> B.ByteString -> B.ByteString
xorFixed p k = B16.encode $ B.pack $ B.zipWith xor (head bs) (last bs)
  where bs = fmap (fst . B16.decode) [p, k]
