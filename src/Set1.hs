{-# LANGUAGE OverloadedStrings #-}

module Set1 where

import           Data.Bits       (shift, (.&.), (.|.))
import           Data.ByteString (ByteString, pack, unpack)
import qualified Data.ByteString as LBS
import           Data.Char       (ord)
import           Data.List.Split (chunksOf)
import           Data.Monoid     ((<>))
import           GHC.Word        (Word8)

data Error = NotHex Char
           | UnevenNumberOfHexDigits
             deriving (Eq, Show)

challenge1 :: String -> Either Error String
challenge1 = (fmap bsToBase64) . hexToByteString

hexToByteString :: String -> Either Error ByteString
hexToByteString s =
  let hexDigitsToWord8 [a,b] = Right $ shift a 4 .|. b
      -- If we have an odd number of hex chars the last chunk will have only one element
      -- so we need to pad the 4 most significant bits with 0
      hexDigitsToWord8 [a]   = hexDigitsToWord8 [0, a]
      hexDigitsToWord8 _     = Left UnevenNumberOfHexDigits
   in fmap (pack . concat . traverse hexDigitsToWord8 . chunksOf 2) $ traverse hexCharToWord8 s

hexCharToWord8 :: Char -> Either Error Word8
hexCharToWord8 c
  | c `elem` ['0'..'9'] = Right . fromIntegral $ ord c - ord '0'
  | c `elem` ['a'..'f'] = Right . fromIntegral $ ord c - ord 'a' + 10
  | c `elem` ['A'..'F'] = Right . fromIntegral $ ord c - ord 'A' + 10
  | otherwise           = Left (NotHex c)

bsToBase64 :: ByteString -> String
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
  in concat . fmap bytesToB64 . chunksOf 3 . unpack

bsToHex :: ByteString -> String
bsToHex "\NUL" = "0"
bsToHex bs =
  let f b s = foldr (:) s $ byteToHex b
   in LBS.foldr f "" bs

byteToHex :: Word8 -> [Char]
byteToHex b =
   fmap bottom4BitsToHex [top 4 b, b]

bottom4BitsToHex :: Word8 -> Char
bottom4BitsToHex =
  let hexChars = ['0'..'9'] <> ['a'..'f']
   in (hexChars !!) . fromIntegral . bottom 4

sixBitsToB64 :: Word8 -> Char
sixBitsToB64 =
  let base64Chars = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> ['+','-']
   in (base64Chars !!) . fromIntegral

top :: Int -> Word8 -> Word8
top n x = shift x (n - 8)

bottom :: Int -> Word8 -> Word8
bottom n b =
  let e = fromIntegral n :: Double
      mask = (floor (2 ** e)) - 1
   in b .&. mask

bottomNTo6 :: Int -> Word8 -> Word8
bottomNTo6 n b = shift (bottom n b) (6 - n)
