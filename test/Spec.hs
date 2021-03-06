{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map

import Lib

tests :: FrequencyTable -> TestTree
tests english = testGroup "Tests" [unitTests english]

unitTests :: FrequencyTable -> TestTree
unitTests english = testGroup "Unit tests" [
  testCase "https://cryptopals.com/sets/1/challenges/1 (hexToBase64)" $
    hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    @?= "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t",

  testCase "https://cryptopals.com/sets/1/challenges/2 (xor/fixed)" $
    xor (fromBase16 "1c0111001f010100061a024b53535009181c")
        (fromBase16 "686974207468652062756c6c277320657965")
    @?= fromBase16 "746865206b696420646f6e277420706c6179",

  testCase "https://cryptopals.com/sets/1/challenges/3 (histogram)" $
    histogram (ngrams "foobar" 1)
    @?= Map.fromList [("a",1),("b",1),("f",1),("o",2),("r",1)],

  testCase "https://cryptopals.com/sets/1/challenges/3 (frequencies)" $
    (frequencies . histogram . flip ngrams 1) "foobar"
    @?= Map.fromList [("a",1/6),("b",1/6),("f",1/6),("o",2/6),("r",1/6)],

  keyScoresTest english,

  testCase "https://cryptopals.com/sets/1/challenges/4 (xor/cycle)" $
    xor "ICE" "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    @?= fromBase16 "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f",

  testCase "https://cryptopals.com/sets/1/challenges/6 (hammingDistance)" $ sequence_ [
    hammingDistance (BS.unpack "this is a test") (BS.unpack "wokka wokka!!!") @?= 37,
    hammingDistance ([254] :: [Int]) ([255] :: [Int]) @?= 1,
    hammingDistance ([001] :: [Int]) ([002] :: [Int]) @?= 2,
    hammingDistance ([0b1] :: [Int]) ([0b0] :: [Int]) @?= 1,
    hammingDistance ([0b0001, 0b0010] :: [Int])
                    ([0b0010, 0b1000] :: [Int]) @?= 2 + 2],

  testCase "https://cryptopals.com/sets/1/challenges/6 (slice)" $ sequence_ [
    slice 5 5 "abcde" @?= (["abcde"],                 ""),
    slice 5 1 "abcde" @?= (["abcde"],                 "bcde"),
    slice 1 1 "abcde" @?= (["a", "b", "c", "d", "e"], ""),
    slice 1 2 "abcde" @?= (["a", "c", "e"],           ""),
    slice 2 2 "abcde" @?= (["ab", "cd"],              "e"),
    slice 2 1 "abcde" @?= (["ab", "bc", "cd", "de"],  "e"),
    slice 0 1 "abcde" @?= ([],                        "abcde"),
    slice 1 0 "abcde" @?= ([],                        "abcde"),
    slice 0 1 "abcde" @?= ([],                        "abcde")
  ]]

keyScoresTest :: FrequencyTable -> TestTree
keyScoresTest ref = testCase "https://cryptopals.com/sets/1/challenges/3 (keyScores)" $
  elem "X" (fst <$> take 5 ks) @?= True where
    ks = keyScores (genKeys 1) ref cipher
    cipher = fromBase16 "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"


main :: IO ()
main = do
  ns <- decodeNgramsFromFile "ngrams.csv"
  case ns of
    Left err -> putStrLn err
    Right v -> defaultMain $ tests $ ngramFrequencies v
