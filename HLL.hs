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

import           Data.Bits                  as DB
import qualified Data.Bits.Extras           as BE (nlz)
import qualified Data.ByteString.Lazy.Char8 as BC (pack)
import qualified Data.Digest.XXHash         as XXH (xxHash)
import qualified Data.List                  as DL (nub)
import           Data.Word                  (Word32)

-- Splits Word32 to two Word16s
split ss x = (fromIntegral (shiftR ix ss .&. mask), fromIntegral (ix .&. mask))
  where ix = toInteger x
        mask = 2 ^ ss - 1

-- hash value in {0, 1} (32-bits)
mkHash :: Fractional a => String -> a
mkHash s = fromIntegral h / fromIntegral (maxBound :: Word32)
   where h = XXH.xxHash $ BC.pack s

-- Initialize M. Needs to be an array as aggregate modifies random values in the list
initM m = replicate 0

-- Phase 1: Aggregation
-- map to individual M values
-- return 0 if more than p(s)
aggregate v M = max (M !! j) p
  where x = mkHash v
        xp = split 16 x -- 16 should probably be p value. that is, configurable
        -- Flajolet = "1 +", Google = no "1 +"
        j = 1 + fst xp -- {the binary address determined by the first b/p bits of x}. idx in Google paper.
        w = snd xp -- second bits of h
        p = BE.nlz w + 1 -- the position of the leftmost 1-bit

-- Phase 2: Result computation
-- E: the “raw” HyperLogLog estimate}
resultComp a m M
  | E <= 5/2 * m = if V == 0 then E else linearCounting m V -- {small range correction}
  | E <= 1/30 * 2 ^ 32 = E -- {intermediate range -- no correction}
  | otherwise = -2 ^ 32 * log (1 - (E / 2 ^ 32)) -- {large range correction}
  where
    calcAlpha m
     | m == 16 = 0.673
     | m == 32 = 0.697
     | m == 64 = 0.709
     | otherwise = 0.7213 / (1 + 1.079 / m) -- Intended for m >= 128
    -- Let V be the number of registers equal to 0.
    V = length $ filter (== 0) M
    c M j = (sum M * 2 ^ -(last M)) ^ (-1)
    E = calcAlpha m * m ^ 2 * c M j
    linearCounting = m * log (m / V)

main :: IO ()
main = do
    let filename = "google-10000-english.txt"
    text <- filter (notElem '.') <$> concatMap words <$> lines <$> readFile filename

    -- Phase 0: Initialization
    -- b is referred to as p in the Google paper
    let b = 4 -- TODO: Need to figure out what to set this to.
    let m = 2 ^ b -- b from the set [4..16]

    let h = map mkHash text
    let V = undefined
    let M = aggregate h $ initM m

    putStr "Total number of words: "
    --print $ length hashed
    --let dHashed = DL.nub hashed
    putStr "Distinct number of words: "
    --print $ length dHashed

