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
import qualified Control.Monad.ST            as CMS (ST, runST)
import           Data.Bits                   as DB
import qualified Data.Bits.Extras            as BE (nlz, w16)
import qualified Data.ByteString.Lazy.Char8  as BC (pack)
import qualified Data.Digest.XXHash          as XXH (xxHash)
import qualified Data.Vector.Unboxed         as DVU (Vector, filter, freeze,
                                                     length, map, sum)
import qualified Data.Vector.Unboxed.Mutable as DVUM (read, replicate, write)
import           Data.Word                   (Word16, Word32)

-- Splits Word32 into two Word16s
split :: Int -> Word32 -> (Word16, Word16)
split ss x = (fromIntegral (shiftR x ss .&. mask), fromIntegral (x .&. mask))
  where mask = 2 ^ ss - 1 -- Note: Should hard code these values if you aren't going to support variable return types

-- hash value in {0, 1} (32-bits)
mkHash :: String -> Word32
mkHash s = XXH.xxHash $ BC.pack s

calcP :: String -> Int -> (Word16, Word16)
calcP x m = (j, p)
    where h = div (mkHash x) (fromIntegral m)
          hs = split 16 h -- 16 should probably be p value. that is, configurable
          -- Flajolet = "1 +", Google = no "1 +"
          j = BE.w16 $ succ $ fst hs -- {the binary address determined by the first b/p bits of x}. idx in Google paper.
          w = snd hs -- second bits of h
          p = BE.w16 $ succ $ BE.nlz w -- the position of the leftmost 1-bit

-- Phase 1: Aggregation
-- map to individual M values
-- return 0 if more than p(s)
-- Note: Side effects!
aggregate :: CMP.PrimMonad m => [String] -> Int -> m (DVU.Vector Word16)
aggregate vs n = do
  mv <- DVUM.replicate n 0 -- $ BE.w16 0
  CM.forM_ vs $ \v -> do
     let (j, p) = calcP v n
     let ji = fromIntegral j
     jv <- DVUM.read mv ji
     DVUM.write mv ji $ max jv p
  DVU.freeze mv

calcAlpha :: Int -> Float
calcAlpha x
 | x == 16 = 0.673
 | x == 32 = 0.697
 | x == 64 = 0.709
 | otherwise = 0.7213 / (1 + 1.079 / fromIntegral x) -- Intended for m >= 128

zeroCount :: DVU.Vector Word16 -> Int
zeroCount xs = DVU.length $ DVU.filter (== 0) xs -- Let V be the number of registers equal to 0.

linearCounting :: Floating a => Int -> Int -> a
linearCounting n v = ni * log (ni / vi)
    where ni = fromIntegral n
          vi = fromIntegral v

-- Phase 2: Result computation
-- E: the “raw” HyperLogLog estimate}
-- Note: No side effects but does need to read vector.
resultComp :: DVU.Vector Word16 -> Int -> Float
resultComp mv n
  | e <= 5 / 2 * fromIntegral n = if v == 0 then e else linearCounting n v -- {small range correction}
  | e <= (1 / 30) * 2 ^ 32 = e -- {intermediate range -- no correction}
  | otherwise = -2 ^ 32 * log (1 - (e / 2 ^ 32)) -- {large range correction}
  where
    v = zeroCount mv
    sigma = DVU.sum $ DVU.map (\j -> 2 ^^ (-(fromIntegral j))) mv
    e = calcAlpha n * fromIntegral n ^ 2 * sigma ^^ (-1)

-- vector computation in IO
goIO :: [String] -> Int -> IO (DVU.Vector Word16)
goIO = aggregate

-- vector computation in ST
goST :: [String] -> Int -> CMS.ST s (DVU.Vector Word16)
goST = aggregate

main :: IO ()
main = do
    let filename = "google-10000-english.txt"
    text <- filter (notElem '.') <$> concatMap words <$> lines <$> readFile filename

    -- Phase 0: Initialization
    -- b is referred to as p in the Google paper
    let b = 16 -- TODO: Need to figure out what to set this to.
    let m = 2 ^ b -- b from the set [4..16]

    let mv = CMS.runST $ goST text m
    let r = resultComp mv m

    putStr "Total number of words: "
    print $ length text
    --let dHashed = DL.nub hashed
    putStr "Distinct number of words: "
    print r
