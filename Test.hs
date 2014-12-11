module Test where

import Data.Bits as DB
import qualified Data.Char as DC (intToDigit)
import Data.Word (Word32,Word16)
import qualified Numeric   as N (showIntAtBase)
import qualified Data.Digest.XXHash          as XXH (xxHash)
import qualified Data.ByteString.Lazy.Char8  as BC (pack)

import Data.ReinterpretCast

-- Splits Word32 to two Word16s
split :: Float -> (Word16, Word16)
split x = (fromIntegral (shiftR ix 16 .&. mask), fromIntegral (ix .&. mask))
  where ix = floatToWord x
        mask = 65535 -- 2 ^ 16 - 1

mkHash :: String -> Float
mkHash s = fromIntegral h / fromIntegral (maxBound :: Word32)
   where h = XXH.xxHash $ BC.pack s

--split32 :: (Double t, Bits t) => t -> (t, t)
split32 x = (shiftR x 16 DB..&. mask, x DB..&. mask)
   where mask = 65535 -- 2 ^ 16 - 1
--
-- splitWord32 :: Word32 -> (Word16, Word16)
-- splitWord32 x = (fromIntegral (shiftR x 16) .&. maxBound, fromIntegral x)

-- Returns binary representation of number
toBase :: (Integral a, Show a) => a -> a -> String
toBase base num = N.showIntAtBase base DC.intToDigit num ""

main :: IO ()
main = do
    -- let x = maxBound :: Word32
    -- show in binary
    let x = mkHash "Hello"
    putStr "hashed value of x: "
    print x
    putStr "x in binary: "
    let toBase2 = toBase 2
    print $ toBase2 $ floatToWord x

    -- split into Word16s
    let w16 = split x
    print w16
    print $ toBase2 $ fromIntegral $ fst w16
    print $ toBase2 $ fromIntegral $ snd w16

