{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map
import Data.Char (ord)
import Data.Ratio

import Lib

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests" [
  testCase "https://cryptopals.com/sets/1/challenges/1 (hexToBase64)" $
    hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    @?= "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t",

  testCase "https://cryptopals.com/sets/1/challenges/2 (xor/fixed)" $
    xor (fromBase16 "1c0111001f010100061a024b53535009181c")
        (fromBase16 "686974207468652062756c6c277320657965")
    @?= fromBase16 "746865206b696420646f6e277420706c6179",

  testCase "https://cryptopals.com/sets/1/challenges/3 (xor/cycle)" $
    xor "1111111" "KEY" @?= "zthzthz",

  testCase "https://cryptopals.com/sets/1/challenges/3 (histogram)" $
    histogram "foobar"
    @?= Histogram (Map.fromList [(97,1),(98,1),(102,1),(111,2),(114,1)]),

  testCase "https://cryptopals.com/sets/1/challenges/3 (frequencies)" $
    (frequencies . histogram) "foobar"
    @?= FrequencyTable (Map.fromList [
      (97,1 % 6),(98,1 % 6),(102,1 % 6),(111,1 % 3),(114,1 % 6)]),

  testCase "https://cryptopals.com/sets/1/challenges/3 (perms)" $
    perms 2 (fromIntegral . ord <$> ['a', 'b'])
    @?= ["aa", "ab", "ba", "bb"],

  testCase "https://cryptopals.com/sets/1/challenges/3 (similarity, kernel)" $
    similarity english (frequencies $ histogram $ kernel english)
    @?= 1,

  testCase "https://cryptopals.com/sets/1/challenges/3 (distances)" $
    distances english (frequencies $ histogram "e")
    @?= Map.fromList [(fromIntegral $ ord 'e', 1 - 0.1249)],

  testCase "https://cryptopals.com/sets/1/challenges/3 (xorKey/0)" $
    xorKey english 0 "foo"
    @?= "",

  testCase "https://cryptopals.com/sets/1/challenges/3 (xorKey/1)" $
    xorKey english 1 (fromBase16 "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
    @?= "X"
  ]

main :: IO ()
main = defaultMain tests
