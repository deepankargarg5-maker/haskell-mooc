{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Set14a where
import Mooc.Todo
import Data.Bits
import Data.Char
import Data.Text.Encoding
import Data.Word
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

------------------------------------------------------------------------------
-- Ex 1:
-- Greet with truncation if >15 chars
greetText :: T.Text -> T.Text
greetText name =
  if T.length name > 15
  then T.concat [T.pack "Hello, ", T.take 15 name, T.pack "...!"]
  else T.concat [T.pack "Hello, ", name, T.pack "!"]

------------------------------------------------------------------------------
-- Ex 2:
-- Capitalize every second word (0-indexed even)
shout :: T.Text -> T.Text
shout txt = T.unwords $ zipWith f [0..] (T.words txt)
  where
    f i w = if even i then T.toUpper w else w

------------------------------------------------------------------------------
-- Ex 3:
-- Longest sequence of repeating char in Text
longestRepeat :: T.Text -> Int
longestRepeat txt
  | T.null txt = 0
  | otherwise = maximum . map T.length . T.group $ txt

------------------------------------------------------------------------------
-- Ex 4:
-- Take first n characters from lazy Text as strict Text
takeStrict :: Int64 -> TL.Text -> T.Text
takeStrict n = TL.toStrict . TL.take n

------------------------------------------------------------------------------
-- Ex 5:
-- Difference between max and min byte value in ByteString, 0 if empty
byteRange :: B.ByteString -> Word8
byteRange bs
  | B.null bs = 0
  | otherwise = let mx = B.maximum bs
                    mn = B.minimum bs
                in mx - mn

------------------------------------------------------------------------------
-- Ex 6:
-- XOR checksum via fold with xor starting at 0
xorChecksum :: B.ByteString -> Word8
xorChecksum = B.foldl' xor 0

------------------------------------------------------------------------------
-- Ex 7:
-- Count UTF-8 characters if valid, else Nothing
countUtf8Chars :: B.ByteString -> Maybe Int
countUtf8Chars bs = case decodeUtf8' bs of
  Left _ -> Nothing
  Right txt -> Just (T.length txt)

------------------------------------------------------------------------------
-- Ex 8:
-- Infinite lazy ByteString: b ++ reversed b ++ b ++ reversed b ...
pingpong :: B.ByteString -> BL.ByteString
pingpong b =
  let revB = B.reverse b
      cycleChunks = cycle [b, revB]
  in BL.fromChunks cycleChunks
