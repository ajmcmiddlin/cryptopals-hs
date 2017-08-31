{-# LANGUAGE OverloadedStrings #-}

module Set1 ( -- * Types
              Hex (unHex)
            , HexError
            , mkHex
            , Base64 (unBase64)
            , Base64Error
            , mkBase64
              -- * Challenges
            , challenge1
            , challenge2
              -- * Hex conversion
            , hexToByteString
            , bsToHex
            , canonicalHex
            )
            where

import           Data.Bits       (shift, (.&.), (.|.), xor)
import           Data.Bool       (bool)
import           Data.ByteString (ByteString, pack, unpack)
import qualified Data.ByteString as LBS
import           Data.Char       (ord, toUpper)
import           Data.List.Split (chunksOf)
import           Data.Monoid     ((<>))
import           GHC.Word        (Word8)

data Error = BadHex HexError
           | UnevenNumberOfHexDigits
             deriving (Eq, Show)

-- HEX STUFF
------------
newtype Hex = Hex {unHex :: String}
              deriving (Eq, Show)

hexChars :: [Char]
hexChars = ['0'..'9'] <> ['a'..'f'] <> ['A'..'F']

mkHex :: String -> Either HexError Hex
mkHex =
  let mkHexChar c
        | c `elem` hexChars = Right c
        | otherwise = Left (NotHex c)
   in fmap Hex . traverse mkHexChar

data HexError = NotHex Char
                deriving (Eq, Show)

-- BASE64 STUFF
---------------
newtype Base64 = Base64 {unBase64 :: String}
                 deriving (Eq, Show)

base64Chars :: [Char]
base64Chars = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> ['+','-']

mkBase64 :: String -> Either Base64Error Base64
mkBase64 =
  let mkBase64Char c
        | c `elem` base64Chars = Right c
        | otherwise = Left (NotBase64 c)
   in fmap Base64 . traverse mkBase64Char

data Base64Error = NotBase64 Char
                   deriving (Eq, Show)

-- CHALLENGES
-------------
challenge1 :: Hex -> Either Error Base64
challenge1 = (fmap bsToBase64) . hexToByteString

challenge2 :: Hex -> Hex -> Either Error Hex
challenge2 h1 h2 = do
  bs1 <- hexToByteString h1
  bs2 <- hexToByteString h2
  pure . bsToHex . pack $ LBS.zipWith xor bs1 bs2

hexToByteString :: Hex -> Either Error ByteString
hexToByteString s =
  let s' = unHex s
      s'' = bool ('0':s') s' (even . length $ s')
      hexDigitsToWord8 [a,b] = Right $ shift a 4 .|. b
      hexDigitsToWord8 _     = Left UnevenNumberOfHexDigits
   in fmap (pack . concat . traverse hexDigitsToWord8 . chunksOf 2) $ traverse hexCharToWord8 s''

hexCharToWord8 :: Char -> Either Error Word8
hexCharToWord8 c
  | c `elem` ['0'..'9'] = Right . fromIntegral $ ord c - ord '0'
  | c `elem` ['a'..'f'] = Right . fromIntegral $ ord c - ord 'a' + 10
  | c `elem` ['A'..'F'] = Right . fromIntegral $ ord c - ord 'A' + 10
  | otherwise           = Left . BadHex $ NotHex c

bsToBase64 :: ByteString -> Base64
bsToBase64 =
  let bytesToB64 [a,b,c] = fmap sixBitsToB64
        [ (top 6 a)
        , (bottomNTo6 2 a .|. top 4 b)
        , (bottomNTo6 4 b .|. top 2 c)
        , (bottomNTo6 6 c)
        ]
      bytesToB64 [a,b] = (<> "=") . take 3 $ bytesToB64 [a,b,0]
      bytesToB64 [a] = (<> "==") . take 2 $ bytesToB64 [a,0,0]
      bytesToB64 _ = error "You got an empty chunk, or >3 chunks. Sorry?"
  in Base64 . concat . fmap bytesToB64 . chunksOf 3 . unpack

bsToHex :: ByteString -> Hex
bsToHex "\NUL" = Hex "0"
bsToHex bs =
  let f b s = foldr (:) s $ byteToHex b
   in canonicalHex . Hex $ LBS.foldr f "" bs

canonicalHex :: Hex -> Hex
canonicalHex h@(Hex "") = h
canonicalHex h = Hex
               . fmap toUpper
               . (\s' -> bool s' "0" (s' == ""))
               . dropWhile (== '0')
               . unHex
               $ h

byteToHex :: Word8 -> [Char]
byteToHex b =
   fmap bottom4BitsToHex [top 4 b, b]

bottom4BitsToHex :: Word8 -> Char
bottom4BitsToHex =
   (hexChars !!) . fromIntegral . bottom 4

sixBitsToB64 :: Word8 -> Char
sixBitsToB64 =
  (base64Chars !!) . fromIntegral

top :: Int -> Word8 -> Word8
top n x = shift x (n - 8)

bottom :: Int -> Word8 -> Word8
bottom n b =
  let e = fromIntegral n :: Double
      mask = (floor (2 ** e)) - 1
   in b .&. mask

bottomNTo6 :: Int -> Word8 -> Word8
bottomNTo6 n b = shift (bottom n b) (6 - n)
