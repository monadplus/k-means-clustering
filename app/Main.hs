{-# LANGUAGE ScopedTypeVariables        #-}
module Main where

import           KMeans
import qualified Data.Vector as Vector
import           Data.Time            (diffUTCTime, getCurrentTime)
import           Text.Printf

main :: IO ()
main = do
  (r, time) <- timeit $ pure $ KMeans.fit' (KMeans.generateRandomPoints 10000) 3
  Vector.ifoldM
    (\_ i (Cluster points) -> printf "%d th-cluster has %d elements\n" i (length points))
    ()
    (KMeans.clusters r)
  printf "KMeans(k = 10, n = 1000): %.2fs\n" time


-- | Return the computation and the time
timeit :: IO a -> IO (a,Double)
timeit io = do
   t0 <- getCurrentTime
   a <- io
   t1 <- getCurrentTime
   return (a, realToFrac (t1 `diffUTCTime` t0))
