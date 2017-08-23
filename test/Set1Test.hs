module Set1Test where

import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

import Set1 (challenge1)

test_Set1 :: TestTree
test_Set1 =
  testGroup "Set1" [
    challenge1Tests
  ]

challenge1Input :: String
challenge1Input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

challenge1Expected :: String
challenge1Expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

challenge1Tests :: TestTree
challenge1Tests =
  testGroup "Challenge1" [
    testCase "Answer" $ challenge1 challenge1Input @?= Right challenge1Expected
  ]
