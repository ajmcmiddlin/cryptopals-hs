module Set1Test where

import           Data.ByteString.Char8 (pack)
import           Hedgehog              (forAll, property, (===))
import qualified Hedgehog.Gen          as Gen
import           Hedgehog.Internal.Gen (MonadGen)
import qualified Hedgehog.Range        as Range
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.Hedgehog   (testProperty)
import           Test.Tasty.HUnit      (testCase, (@?=))

import           Set1                  (challenge1, hexToByteString, bsToHex)

test_Set1 :: TestTree
test_Set1 =
  testGroup "Set1" [
    challenge1Tests
  ]

challenge1Input :: String
challenge1Input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

challenge1Expected :: String
challenge1Expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

hexString :: MonadGen m => m String
hexString = Gen.list (Range.linear 0 100) Gen.hexit

challenge1Tests :: TestTree
challenge1Tests =
  testGroup "Challenge1" [
    testCase "Answer" $
      challenge1 challenge1Input @?= Right challenge1Expected
  , testCase "hexToByteString" $
      hexToByteString challenge1Input @?= Right (pack "I'm killing your brain like a poisonous mushroom")
  , testProperty "hex round trip" (property $ do
      hs <- forAll hexString
      Right hs === (bsToHex <$> (hexToByteString hs)))
  ]
