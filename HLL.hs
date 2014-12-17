-- HyperLogLog: the analysis of a near-optimal cardinality estimation algorithm:
--  http://algo.inria.fr/flajolet/Publications/FlFuGaMe07.pdf)
-- Sketch of the Day: HyperLogLog — Cornerstone of a Big Data Infrastructure:
--  http://research.neustar.biz/2012/10/25/sketch-of-the-day-hyperloglog-cornerstone-of-a-big-data-infrastructure/

-- HyperLogLog in Practice: Algorithmic Engineering of a State of The Art Cardinality Estimation Algorithm:
--  http://static.googleusercontent.com/external_content/untrusted_dlcp/research.google.com/en/us/pubs/archive/40671.pdf
-- HyperLogLog++: Google’s Take On Engineering HLL:
--  http://research.neustar.biz/2013/01/24/hyperloglog-googles-take-on-engineering-hll/?blogsub=subscribed#subscribe-blog

module HLL where

import qualified Control.Monad               as CM (forM_)
import qualified Control.Monad.ST            as CMS (runST)
import           Data.Bits
-- from bits-extras
import qualified Data.Bits.Extras            as BE (trailingZeros)
import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString.Lazy.Char8  as BC (pack)
import qualified Data.Digest.XXHash          as XXH (xxHash)
import qualified Data.Vector.Unboxed         as DVU (Vector, filter, freeze,
                                                     length, map, sum, zipWith)
import qualified Data.Vector.Unboxed.Mutable as DVUM (read, replicate, write)
import qualified Data.Word                   as DW (Word32)

import qualified Data.Binary                 as DB (encode)
import qualified Data.List.Split             as DLS (chunksOf)
import           System.Random

calcM :: Int -> Int
calcM b = 2 ^ b

-- Phase 1: Aggregation
aggregate :: [BS.ByteString] -> Int -> DVU.Vector DW.Word32
aggregate vs b = CMS.runST $ do
  reg <- DVUM.replicate (calcM b) 0
  CM.forM_ vs $ \v -> do
     let (j, rho) = mkPair v b
     jv <- DVUM.read reg j
     DVUM.write reg j $ max jv rho
  DVU.freeze reg

mkPair :: BS.ByteString -> Int -> (Int, DW.Word32)
mkPair v b = (j, rho)
    where mask = pred $ 1 `shiftL` b
          h = XXH.xxHash v
          j = fromIntegral $ h .&. mask -- isolate first b bits for use as index
          w = h `shiftR` b -- remove first b bits
          rho = 1 + BE.trailingZeros w -- count leading zeros from lsb (yes, this is confusing)

-- Phase 2: Result computation
calcE :: DVU.Vector DW.Word32 -> Int -> Float
calcE rs m | rawE <= 5 / 2 * mi = if v == 0 then rawE else linearCounting -- {small range correction}
           | rawE <= 1 / 30 * 2 ^ 32 = rawE -- {intermediate range -- no correction}
           | otherwise = -2 ^ 32 * log (1 - rawE / 2 ^ 32) -- {large range correction}
           where mi = fromIntegral m
                 v = DVU.length $ DVU.filter (== 0) rs -- Let V be the number of registers equal to 0.
                 z =  1 / DVU.sum (DVU.map (\r -> 2 ^^ (-(fromIntegral r))) rs)
                 rawE = alpha m * mi ^ 2 * z
                 linearCounting = mi * log (mi / fromIntegral v)

alpha :: Int -> Float
alpha m | d == 1 = 0.673 -- m >= 16
        | d == 2 = 0.697 -- m >= 32
        | d >= 4 && d < 8 = 0.709 -- m >= 64
        | d >= 8 = 0.7213 / succ (1.079 / fromIntegral m) -- m >= 128 (in paper)
        | otherwise = 1
        where d = div m 16

-- b from the set [4..16]
card :: [BS.ByteString] -> Int -> Int
card vs b = round $ calcE e m
    where e = aggregate vs b
          m = calcM b

card' :: [String] -> Int -> Int
card' vs = card (map BC.pack vs)

-- Contrived in that you could just feed it all as one list. Just using to test union approach for now.
union :: [BS.ByteString]
           -> [BS.ByteString] -> Int -> Int
union bs1 bs2 b = round $ calcE vsc m
    where m = calcM b
          vs1 = aggregate bs1 b
          vs2 = aggregate bs2 b
          vsc = DVU.zipWith max vs1 vs2

main :: IO ()
main = do
    let b = 16

    g <- getStdGen
    let n = floor 1e6 -- Max value tested. 1e7 kills GHCI.
    let c = div n 5
    let r = take n (randoms g :: [DW.Word32])
    let bss = DLS.chunksOf c $ map DB.encode r
    let cards = map (`card` b) bss
    print cards

    let results = map (\x -> fromIntegral c / fromIntegral x)

    print $ results cards

    let unionCards = zipWith (\x y -> union x y b) bss $ tail bss
    print unionCards

    print $ map (* 2) $ results unionCards


