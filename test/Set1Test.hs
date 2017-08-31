module Set1Test where

import           Data.ByteString.Char8 (pack)
import           Hedgehog              (forAll, property, (===))
import qualified Hedgehog.Gen          as Gen
import           Hedgehog.Internal.Gen (MonadGen)
import qualified Hedgehog.Range        as Range
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.Hedgehog   (testProperty)
import           Test.Tasty.HUnit      (testCase, (@?=))

import           Set1                  (Hex, Base64, bsToHex, canonicalHex, challenge1,
                                        challenge2, hexToByteString, mkBase64,
                                        mkHex)

test_Set1 :: TestTree
test_Set1 =
  testGroup "Set1" [
    hexTests
  , challenge1Tests
  , challenge2Tests
  ]

challenge1Input ::Hex
challenge1Input = naughtyHex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

challenge1Expected :: Base64
challenge1Expected = naughtyBase64 "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

hexString :: MonadGen m => m Hex
hexString = fmap (naughtyDeEither . mkHex) $ Gen.list (Range.linear 0 100) Gen.hexit

hexTests :: TestTree
hexTests =
  testGroup "Hex" [
    testProperty "hex round trip" . property $ do
      hs <- canonicalHex <$> forAll hexString
      fmap bsToHex (hexToByteString hs) === Right hs
  , testProperty "ByteString to hex round trip" . property $ do
      bs <- forAll (Gen.bytes (Range.linear 0 1024))
      hexToByteString (bsToHex bs) === Right bs
  ]

challenge1Tests :: TestTree
challenge1Tests =
  testGroup "Challenge1" [
    testCase "Answer" $
      challenge1 challenge1Input @?= Right challenge1Expected
  , testCase "hexToByteString" $
      hexToByteString challenge1Input @?= Right (pack "I'm killing your brain like a poisonous mushroom")
  ]

challenge2Tests :: TestTree
challenge2Tests =
  testCase "Challenge2" $
    let h1 = naughtyHex "1c0111001f010100061a024b53535009181c"
        h2 = naughtyHex "686974207468652062756c6c277320657965"
     in challenge2 h1 h2 @?= Right (canonicalHex (naughtyHex "746865206b696420646f6e277420706c6179"))

naughtyHex :: String -> Hex
naughtyHex = naughtyDeEither . mkHex

naughtyBase64 :: String -> Base64
naughtyBase64 = naughtyDeEither . mkBase64

naughtyDeEither :: Either a b -> b
naughtyDeEither = either (error "You can't be trusted with naughty") id
