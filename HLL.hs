-- HyperLogLog: the analysis of a near-optimal cardinality estimation algorithm:
--  http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf)
-- Sketch of the Day: HyperLogLog — Cornerstone of a Big Data Infrastructure:
--  http://research.neustar.biz/2012/10/25/sketch-of-the-day-hyperloglog-cornerstone-of-a-big-data-infrastructure/

-- HyperLogLog in Practice: Algorithmic Engineering of a State of The Art Cardinality Estimation Algorithm:
--  http://static.googleusercontent.com/external_content/untrusted_dlcp/research.google.com/en/us/pubs/archive/40671.pdf
-- HyperLogLog++: Google’s Take On Engineering HLL:
--  http://research.neustar.biz/2013/01/24/hyperloglog-googles-take-on-engineering-hll/?blogsub=subscribed#subscribe-blog

module HLL where

import           Control.Applicative
import qualified Control.Monad               as CM (forM_)
import qualified Control.Monad.Primitive     as CMP (PrimMonad)
import qualified Control.Monad.ST            as CMS (runST)
import qualified Data.Bits.Bitwise           as DBB (splitAt)
-- from bits-extras
import qualified Data.Bits.Extras            as BE (trailingZeros)
import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString.Lazy.Char8  as BC (pack)
import qualified Data.Char                   as DC (intToDigit)
import qualified Data.Digest.XXHash          as XXH (xxHash)
import qualified Data.Vector.Unboxed         as DVU (Vector, filter, freeze,
                                                     length, map, sum)
import qualified Data.Vector.Unboxed.Mutable as DVUM (read, replicate, write)
import qualified Data.Word                   as DW (Word32)
import qualified Numeric                     as N (showIntAtBase)

-- Returns binary representation of number
toBase :: (Integral a, Show a) => a -> a -> String
toBase base num = N.showIntAtBase base DC.intToDigit num ""

-- Phase 1: Aggregation
aggregate :: CMP.PrimMonad m => [BS.ByteString] -> Int -> m (DVU.Vector DW.Word32)
aggregate vs b = do
  let n = 2 ^ b
  let bi = fromIntegral b
  reg <- DVUM.replicate n 0
  CM.forM_ vs $ \v -> do
     let h = XXH.xxHash v
     let (j, w) = DBB.splitAt (b - 1) $ fromIntegral h
     let lz = BE.trailingZeros w
     let rho = succ $ if lz == 0 || lz >= bi then 0 else lz -- TODO: always at least 1??? Read paper again.
     jv <- DVUM.read reg j
     DVUM.write reg j $ max jv rho
  DVU.freeze reg

alpha :: Int -> Float
alpha n
 | n == 16 = 0.673
 | n == 32 = 0.697
 | n == 64 = 0.709
 | otherwise = 0.7213 / (1 + 1.079 / fromIntegral n) -- Intended for m >= 128

-- Phase 2: Result computation
calcE :: DVU.Vector DW.Word32 -> Int -> Float
calcE rs n
  | rawE <= 5 / 2 * ni = if v == 0 then rawE else linearCounting -- {small range correction}
  | rawE <= (1 / 30) * 2 ^ 32 = rawE -- {intermediate range -- no correction}
  | otherwise = -2 ^ 32 * log (1 - rawE / 2 ^ 32) -- {large range correction}
  where
    ni = fromIntegral n
    v = DVU.length $ DVU.filter (== 0) rs -- Let V be the number of registers equal to 0.
    z =  1 / DVU.sum (DVU.map (\r -> 2 ^^ (-(fromIntegral r))) rs)
    rawE = alpha n * ni ^ 2 * z
    linearCounting = ni * log (ni / fromIntegral v)

-- b from the set [4..16]
card :: [BS.ByteString] -> Int -> Int
card vs b = do
    let e = CMS.runST $ aggregate vs b
    round $ calcE e (2 ^ b) -- aka m in the literature

main :: IO ()
main = do
    let filename = "google-10000-english.txt"
    text <- map BC.pack <$> filter (notElem '.') <$> concatMap words <$> lines <$> readFile filename

    -- Phase 0: Initialization
    -- b from the set [4..16]
    let b = 16 -- TODO: Make this configurable

    let e = card text b

    putStr "Total number of words: "
    print $ length text
    putStr "Distinct number of words: "
    print e -- return cardinality estimate E with typical relative error ±1.04/ m.
