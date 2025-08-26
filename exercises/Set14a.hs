{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Set14a where

import Data.Bits (xor)
import Data.Char (toUpper)
import Data.Int (Int64)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

------------------------------------------------------------------------------
-- Ex 1
greetText :: T.Text -> T.Text
greetText name
  | T.length name <= 15 = T.concat ["Hello, ", name, "!"]
  | otherwise = T.concat ["Hello, ", T.take 15 name, "...!"]

------------------------------------------------------------------------------
-- Ex 2
shout :: T.Text -> T.Text
shout txt = T.unwords $ zipWith capitalize [0..] (T.words txt)
  where capitalize i word = if even i then T.toUpper word else word

------------------------------------------------------------------------------
-- Ex 3
longestRepeat :: T.Text -> Int
longestRepeat txt
  | T.null txt = 0
  | otherwise = maximum (map T.length (T.group txt))

------------------------------------------------------------------------------
-- Ex 4
takeStrict :: Int64 -> TL.Text -> T.Text
takeStrict n = T.pack . TL.unpack . TL.take n

------------------------------------------------------------------------------
-- Ex 5
byteRange :: B.ByteString -> Word8
byteRange bs
  | B.null bs = 0
  | otherwise = B.maximum bs - B.minimum bs

------------------------------------------------------------------------------
-- Ex 6
xorChecksum :: B.ByteString -> Word8
xorChecksum = B.foldl' xor 0

------------------------------------------------------------------------------
-- Ex 7
countUtf8Chars :: B.ByteString -> Maybe Int
countUtf8Chars bs = case decodeUtf8' bs of
  Left _ -> Nothing
  Right txt -> Just (T.length txt)

------------------------------------------------------------------------------
-- Ex 8
pingpong :: B.ByteString -> BL.ByteString
pingpong b = BL.fromChunks (cycle [b, B.reverse b])
