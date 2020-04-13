{-# LANGUAGE NumericUnderscores #-}
import           Criterion.Main
import           Data.Vector    (Vector)
import           KMeans.Data    (Point(..), generateStandardPoints)
import qualified KMeans0
import qualified KMeans1

-----------------------


main :: IO ()
main = defaultMain [ bgroup "kmeans - version 0"             fitBench0
                   , bgroup "kmeans - version 1"             fitBench1
                   -- bgroup "euclidean distance" euclideanDistanceBench
                   ]

----------------------------------------------------------

standardPoints :: Int -> IO (Vector Point)
standardPoints n = pure (generateStandardPoints n)

fitBench0 :: [Benchmark]
fitBench0 = fitBench (uncurry KMeans0.fit')

fitBench1 :: [Benchmark]
fitBench1 = fitBench (uncurry KMeans1.fit')

fitBench :: KMeans0.NFData a => ((Vector Point, Int) -> a) -> [Benchmark]
fitBench f =
  flip fmap [1_000,10_000,100_000] $ \n ->
    env (standardPoints n) $ \ ~points ->
      bgroup (show n) [
          bgroup "fit" [ bench "2"   $ nf f (points, 2)
                       , bench "10"  $ nf f (points, 10)
                       ]
      ]

----------------------------------------------------------


eu1 :: Point -> Point -> Double
eu1 (Point p1 p2) (Point q1 q2) =
  sqrt . sum . fmap (**2) $ [(q1-p1), (p2-q2)]

eu2 :: Point -> Point -> Double
eu2 (Point p1 p2) (Point q1 q2) =
  sqrt $ ((q1-p1)**2) + ((p2-q2)**2)

eu3 :: Point -> Point -> Double
eu3 (Point p1 p2) (Point q1 q2) =
  sqrt $ ((q1-p1)^(2::Int)) + ((p2-q2)^(2::Int))

-- Not sure if I should generate random points input
euclideanDistanceBench :: [Benchmark]
euclideanDistanceBench =
  let p = (Point 0.3 0.3, Point 0.1 0.1)
   in [ bench "version 1" $ nf (uncurry eu1) p
      , bench "version 2" $ nf (uncurry eu2) p
      , bench "version 3" $ nf (uncurry eu3) p
      ]
