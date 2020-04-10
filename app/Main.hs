{-# LANGUAGE ScopedTypeVariables #-}
module Main where

---------------------------------------------------------------

import           Data.Foldable (traverse_)
import           Data.Time     (diffUTCTime, getCurrentTime)
import qualified Data.Vector   as Vector
import           KMeans
import qualified KMeans.Plot   as Plot
import           Text.Printf
import           Control.Exception (evaluate)
import qualified KMeans.Easy as Easy
import           Control.Monad(forM_)

---------------------------------------------------------------

main :: IO ()
main = do
  let k = (7 :: Int)
      n = (1000 :: Int)
      uniformPoints  = KMeans.generateUniformPoints n
      standardPoints = KMeans.generateStandardPoints n

  forM_ [standardPoints] $ \pointsV -> do
    Easy.plotK "elbow.svg" (Vector.toList pointsV)
    (r, time) <- timeit $ pure $ Easy.easy' pointsV
    printf "KMeans(k = %d, n = %d): %.2fs\n" k n time
    Plot.scatter "kmeans.svg" r


-- | Return the computation and the time
timeit :: (NFData a) => IO a -> IO (a,Double)
timeit io = do
   t0 <- getCurrentTime
   a <- io
   evaluate (rnf a)
   t1 <- getCurrentTime
   return (a, realToFrac (t1 `diffUTCTime` t0))

-- UNUSED
-- | Return info associated with the clustering.
printClusterInfo :: KMeans -> IO ()
printClusterInfo ds =
  traverse_
    (\(i, (Cluster points)) -> printf "%d th-cluster has %d elements\n" i (length points))
    (Vector.indexed $ clusters ds)
