module Mutable where

import qualified Control.Monad               as CM (forM_)
import qualified Control.Monad.Primitive     as CMP (PrimMonad)
import qualified Control.Monad.ST            as CMS

import qualified Data.Vector.Unboxed         as DVU (Vector, freeze)
import qualified Data.Vector.Unboxed.Mutable as DVUM (new, write)

example :: CMP.PrimMonad m => Int -> m (DVU.Vector Int)
example n = do
  v <- DVUM.new 10
  CM.forM_ [0..9] $ \i ->
     DVUM.write v i (2*i)
  DVU.freeze v

-- vector computation in IO
vecIO :: Int -> IO (DVU.Vector Int)
vecIO = example

-- vector computation in ST
vecST :: Int -> CMS.ST s (DVU.Vector Int)
vecST = example

main :: IO ()
main = do
  vecIO 10 >>= print
  print $ CMS.runST (vecST 10)
