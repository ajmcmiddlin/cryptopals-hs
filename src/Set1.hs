module Set1 where

import           Data.Bits       (shift, (.&.), (.|.))
import           Data.Bool       (bool)
import           Data.ByteString (ByteString, pack, unpack)
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
      hexDigitsToWord8 [a]   = hexDigitsToWord8 [a, 0]
      hexDigitsToWord8 _     = Left UnevenNumberOfHexDigits
   in fmap (pack . concat . traverse hexDigitsToWord8 . chunksOf 2) $ traverse hexCharToWord8 s

hexCharToWord8 :: Char -> Either Error Word8
hexCharToWord8 c
  | c `elem` ['0'..'9'] = Right . fromIntegral $ ord c - ord '0'
  | c `elem` ['a'..'f'] = Right . fromIntegral $ ord c - ord 'a' + 10
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

sixBitsToB64 :: Word8 -> Char
sixBitsToB64 w8 =
  (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','-']) !! fromIntegral w8

top :: Int -> Word8 -> Word8
top n x = shift x (n - 8)

bottomNTo6 :: Int -> Word8 -> Word8
bottomNTo6 n x = shift (x .&. (floor (2 ** (fromIntegral n) :: Double) - 1))
                       (6 - n)
