{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Foldable (traverse_)
import           Data.Time     (diffUTCTime, getCurrentTime)
import qualified Data.Vector   as Vector
import           KMeans
import qualified KMeans.Plot   as Plot
import           Text.Printf
import           Control.Exception (evaluate)

main :: IO ()
main = do
  let k = 3
      n = 1000
  (r, time) <- timeit $ pure $ KMeans.fit' (KMeans.generateRandomPoints n) k
  Plot.scatter "result.png" r
  printf "KMeans(k = %d, n = %d): %.2fs\n" k n time


-- | Return the computation and the time
timeit :: (NFData a) => IO a -> IO (a,Double)
timeit io = do
   t0 <- getCurrentTime
   a <- io
   evaluate (rnf a)
   t1 <- getCurrentTime
   return (a, realToFrac (t1 `diffUTCTime` t0))


printClusterInfo :: KMeans -> IO ()
printClusterInfo ds =
  traverse_
    (\(i, (Cluster points)) -> printf "%d th-cluster has %d elements\n" i (length points))
    (Vector.indexed $ clusters ds)
