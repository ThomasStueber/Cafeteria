module ByteMath where

import Data.Int
import Data.Word
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Unsafe.Coerce

reinterpretFloatAsInt :: Float -> Int32
reinterpretFloatAsInt = unsafeCoerce

reinterpretDoubleAsInt :: Double -> Int64
reinterpretDoubleAsInt = unsafeCoerce

reinterpretIntAsFloat :: Int32 -> Float
reinterpretIntAsFloat = unsafeCoerce

reinterpretIntAsDouble :: Int64 -> Double
reinterpretIntAsDouble = unsafeCoerce

fromShort :: Int16 -> [Word8]
fromShort x = [fromIntegral (shiftR x 8), fromIntegral x]

fromInt :: Int32 -> [Word8]
fromInt x = [fromIntegral (shiftR x 24), fromIntegral (shiftR x 16), fromIntegral (shiftR x 8), fromIntegral x]

fromLong :: Int64 -> [Word8]
fromLong x = [fromIntegral (shiftR x 56), fromIntegral (shiftR x 48), fromIntegral (shiftR x 40), fromIntegral (shiftR x 32), fromIntegral (shiftR x 24), fromIntegral (shiftR x 16), fromIntegral (shiftR x 8), fromIntegral x]

fromFloat :: Float -> [Word8]
fromFloat x = fromInt(reinterpretFloatAsInt(x))

fromDouble :: Double -> [Word8]
fromDouble x = fromLong(reinterpretDoubleAsInt(x))

toShort :: [Word8] -> Int16
toShort = foldl rec 0 where rec r x = (shiftL r 8) .|. (fromIntegral x)

toInt :: [Word8] -> Int32
toInt = foldl rec 0 where rec r x = (shiftL r 8) .|. (fromIntegral x)

toLong :: [Word8] -> Int64
toLong = foldl rec 0 where rec r x = (shiftL r 8) .|. (fromIntegral x)

toFloat :: [Word8] -> Float
toFloat x = reinterpretIntAsFloat(toInt(x))

toDouble :: [Word8] -> Double
toDouble x = reinterpretIntAsDouble(toLong(x))

useValueOf = unsafeCoerce

useIntValueOf :: Int32 -> Int
useIntValueOf = unsafeCoerce

useLongValueOf :: Int64 -> Int
useLongValueOf = unsafeCoerce

fromString :: String -> [Word8]
fromString s = B.unpack (U.fromString s)

toString :: [Word8] -> String
toString x = U.toString (B.pack x)

toByteString :: [Word8] -> B.ByteString
toByteString x = B.pack x




-- Benjamin Hellstern, Magdalena Sannwald
-- Seminar Compilerbau WS 14/15
-- WSI Informatik, Ernst-Bloch-Universität Tübingen