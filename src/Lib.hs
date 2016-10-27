{-# LANGUAGE OverloadedStrings #-}

module Lib (hexToBase64) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Base16.Lazy as B16

hexToBase64 :: B.ByteString -> B.ByteString
hexToBase64 = B64.encode . fst . B16.decode
