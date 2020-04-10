{-# LANGUAGE RecordWildCards #-}
module KMeans.Easy (
    easy
  , easy'
  , plotK
  , wss
  ) where


import           Data.List
import           Data.Monoid
import           KMeans
import qualified KMeans.Plot as Plot
import           Data.Vector (Vector)
import qualified Data.Vector as Vector


-- | Given n samples, computes the optimal k that minimizes the within-cluster-sum of squared erros
-- using the elbow method, and returns for each sample its corresponding cluster.
-- The centroid of a cluster is the representative of the cluster.
easy :: [Point] -> KMeans
easy = easy' . Vector.fromList

easy' :: Vector Point -> KMeans
easy' points =
  let kmax = 10
  in fst $ minimumBy
          (\x1 x2 -> snd x1 `compare` snd x2)
          [ let r = fit' points k in (r, wss r)
          | k <- [1..kmax]
          ]

-- | WSS Elbow Plot
--
-- x-axis: k
-- y-axis: wss
plotK :: FilePath -> [Point] -> IO ()
plotK fp points = do
  let kmax = 10
      points' = [ let r = fit points k in (k, wss r)
                | k <- [1..kmax]
                ]
  Plot.elbow fp points'

-- | Compute Within-Cluster-Sum of Squared Errors
--
-- Source: <https://discuss.analyticsvidhya.com/t/what-is-within-cluster-sum-of-squares-by-cluster-in-k-means/2706/2>
wss :: KMeans -> Double
wss KMeans{..} =
  getSum $ foldMap ssc clusters
    where
      ssc cluster =
         let points = getPoints cluster
             Centroid c = getCentroid cluster
         in foldMap (\p -> Sum $ euclideanDistance p c) points
