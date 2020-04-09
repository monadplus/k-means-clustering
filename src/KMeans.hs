{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
module KMeans (
    KMeans(..)
  -- ^ Constructors
  , fit
  , fit'
  , easy
  -- ^ Util
  , wss
  -- ^ Reexports
  , module KMeans.Data
  ) where

-----------------------------------------------------------
-----------------------------------------------------------

import qualified Control.Monad.ST    as ST
import           Data.Coerce
import           Data.List
import qualified Data.Maybe          as Maybe
import           Data.Monoid
import           Data.Vector         (Vector)
import qualified Data.Vector         as Vector
import qualified Data.Vector.Mutable as MVector
import           GHC.TypeLits
import           Positive            (Positive (..))
import qualified Positive
import           KMeans.Data

-----------------------------------------------------------
-----------------------------------------------------------

-- | KMeans DS
data KMeans = KMeans
    { -- points   :: [Point]        -- ^ Points to cluster.
      clusters :: Vector Cluster -- ^ Each cluster is identified by its position in the array.
    }
  deriving (Show)

-- | Given n samples and the number of clusters,
-- computes for each sample its corresponding cluster.
-- The centroid of a cluster is the representative of the cluster.
fit :: [Point] -> Int -> KMeans
fit points = fit' (Vector.fromList points)

fit' :: Vector Point -> Int -> KMeans
fit' points k =
  let it = 100 :: Int -- TODO: fixed for now
      initial = randomAssignment k points
  in KMeans (go it initial)
    where
      go 0 clusters = clusters
      go n clusters = let clusters' = recomputeClusters points clusters
                      in go (n-1) clusters'

recomputeClusters :: Vector Point -> Vector Cluster -> Vector Cluster
recomputeClusters points clusters =
  let centroids = fmap getCentroid clusters
      points' = fmap (\p -> (Positive (nearest p centroids), p)) points
  in computeClusters (length clusters) points'

-- | Returns the id/index of the nearest cluster
nearest :: Point -> Vector Centroid -> Int
nearest p centroids = Vector.minIndex $ fmap (\(Centroid c) -> euclideanDistance p c) centroids

-- | Compute k clusters from the given points and indexes.
computeClusters :: Int -> Vector (Positive, Point) -> Vector Cluster
computeClusters k points = ST.runST $ do
  mvec <- MVector.replicate k []
  Vector.foldM (\_ (Positive i, p) -> MVector.modify mvec ((:) p) i) () points
  Vector.freeze (coerce mvec)

-- | Given the number of clusters and a vector of points, returns a vector of clusters.
-- Each cluster has a list of the points associated with the cluster.
randomAssignment :: Int -> Vector Point -> Vector Cluster
randomAssignment k points =
  let indexes = Positive.generatePositives (length points) . Maybe.fromJust . someNatVal . fromIntegral $ k
  in computeClusters k (Vector.zip indexes points)

-------------------------------------------------------------------
-------------------------------------------------------------------

-- | Given n samples, computes the optimal k that minimizes the within-cluster-sum of squared erros
-- using the elbow method, and returns for each sample its corresponding cluster.
-- The centroid of a cluster is the representative of the cluster.
easy :: [Point] -> KMeans
easy points =
  let kmax = 100
  in fst $ minimumBy
          (\x1 x2 -> snd x1 `compare` snd x2)
          [ let r = fit points k in (r, wss r)
          | k <- [1..kmax]
          ]

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
