{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map
import Data.Char (ord)

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

  testCase "https://cryptopals.com/sets/1/challenges/3 (histogram)" $
    histogram 1 "foobar"
    @?= Histogram (Map.fromList [("a",1),("b",1),("f",1),("o",2),("r",1)]),

  testCase "https://cryptopals.com/sets/1/challenges/3 (frequencies)" $
    (frequencies . histogram 1) "foobar"
    @?= FrequencyTable (Map.fromList [
      ("a",0.16667),("b",0.16667),("f",0.16667),("o",0.33333),("r",0.16667)]),

  testCase "https://cryptopals.com/sets/1/challenges/3 (perms)" $
    perms 2 (fromIntegral . ord <$> ['a', 'b'])
    @?= ["aa", "ab", "ba", "bb"],

  testCase "https://cryptopals.com/sets/1/challenges/3 (similarity, kernel)" $
    similarity english (frequencies $ histogram 1 $ kernel english)
    @?= 1,

  testCase "https://cryptopals.com/sets/1/challenges/3 (xorKeys/0)" $
    xorKeys english 0 "foo"
    @?= [],

  testCase "https://cryptopals.com/sets/1/challenges/3 (xorKeys/1)" $
    head (xorKeys english 1 (fromBase16 "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))
    @?= "X",

  testCase "https://cryptopals.com/sets/1/challenges/4 (xor/cycle)" $
    xor "ICE" "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    @?= fromBase16 "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

  ]

main :: IO ()
main = defaultMain tests
