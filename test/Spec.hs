{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Lib
import qualified Data.Map as Map

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [
    testCase "https://cryptopals.com/sets/1/challenges/1 (hexToBase64)" $
      hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
      @?= "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t",

    testCase "https://cryptopals.com/sets/1/challenges/2 (xor)" $
      xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"
      @?= "746865206b696420646f6e277420706c6179",

    testCase "https://cryptopals.com/sets/1/challenges/3 (histogram)" $
      histogram "foobar"
      @?= Histogram (Map.fromList [(97,1),(98,1),(102,1),(111,2),(114,1)])
  ]

main :: IO ()
main = defaultMain tests
