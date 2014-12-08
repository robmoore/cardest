-- Based on approach outlined in "On Synopses for Distinct-Value Estimation Under Multiset Operations"
-- (http://www.almaden.ibm.com/cs/people/peterh/cacm.pdf) and with directon from the post
-- Sketch of the Day: K-Minimum Values (http://research.neustar.biz/2012/07/09/sketch-of-the-day-k-minimum-values)

module KMV where

import           Control.Applicative

import qualified Data.ByteString.Lazy.Char8 as BC (pack)
import qualified Data.Digest.XXHash         as XXH (xxHash)
import qualified Data.List                  as DL (nub, sort)
import qualified Data.PQueue.Max            as DPM (MaxQueue, deleteMax, empty,
                                                    findMax, insert, null,
                                                    singleton, size, take)
import           Data.Word                  (Word32)

mkHash :: Fractional a => String -> a
mkHash s = fromIntegral h / fromIntegral (maxBound :: Word32)
   where h = XXH.xxHash $ BC.pack s

condInsert :: Ord a => a -> Int -> DPM.MaxQueue a -> DPM.MaxQueue a
condInsert x k mq
   | DPM.null mq = DPM.singleton x
   | DPM.size mq < k = DPM.insert x mq -- haven't reached K so insert
   | x < DPM.findMax mq = DPM.deleteMax $ DPM.insert x mq
   | otherwise = mq -- value isn't larger than max so no-op

estCount :: RealFrac a => Int -> DPM.MaxQueue a -> Int
estCount k mq = if sz < k then sz else round $ d k mq
    where sz = DPM.size mq
          d n pq= fromIntegral (n - 1) / DPM.findMax pq

main :: IO ()
main = do
    let k = 10
    let filename = "google-10000-english.txt"

    text <- filter (notElem '.') <$> concatMap words <$> lines <$> readFile filename
    let hashed = map mkHash text
    putStr "Total number of words: "
    print $ length hashed
    let dHashed = DL.nub hashed
    putStr "Distinct number of words: "
    print $ length dHashed

    let pq = foldr (`condInsert` k) DPM.empty hashed
    putStr "Estimated number of disinct words: "
    print $ estCount k pq
