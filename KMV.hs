-- Based on approach outlined in "On Synopses for Distinct-Value Estimation Under Multiset Operations"
-- (http://www.almaden.ibm.com/cs/people/peterh/cacm.pdf) and with directon from the post
-- Sketch of the Day: K-Minimum Values (http://research.neustar.biz/2012/07/09/sketch-of-the-day-k-minimum-values)

module KMV where

import qualified Data.Digest.XXHash   as XXH (xxHash)
import qualified Data.PQueue.Max      as DPM (MaxQueue, deleteMax, empty,
                                              findMax, insert, null, singleton,
                                              size)
import           Data.Word            (Word32)
import           System.Random

import qualified Data.Binary          as DB (encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.List.Split      as DLS (chunksOf)
import qualified Data.Word            as DW (Word32)

mkValue :: Fractional a => BS.ByteString -> a
mkValue bs = fromIntegral h / fromIntegral (maxBound :: Word32) -- resize to {0,1}
   where h = XXH.xxHash bs

condInsert :: Ord a => a -> Int -> DPM.MaxQueue a -> DPM.MaxQueue a
condInsert v k mq
   | DPM.null mq = DPM.singleton v
   | DPM.size mq < k = DPM.insert v mq -- haven't reached K so insert
   | v < DPM.findMax mq = DPM.deleteMax $ DPM.insert v mq
   | otherwise = mq -- value isn't larger than max so no-op

calcE :: (Fractional a, Integral b) => DPM.MaxQueue a -> b -> a
calcE mq k = fromIntegral (k - 1) / DPM.findMax mq

card :: [BS.ByteString] -> Int -> Int
card bs k = if sz < k then sz else round $ calcE mq k
    where vs = map mkValue bs
          mq = foldr (`condInsert` k) DPM.empty vs
          sz = DPM.size mq

main :: IO ()
main = do
    let k = 100

    g <- getStdGen
    let n = floor 1e6 -- Max value tested. 1e7 kills GHCI.
    let c = div n 10
    let r = take n (randoms g :: [DW.Word32])
    let bss = DLS.chunksOf c $ map DB.encode r
    let cards = map (`card` k) bss
    print cards

    let results = map (\x -> fromIntegral c / fromIntegral x) cards

    print results
