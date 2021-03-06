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
module KMeans0 (
    KMeans(..)
  -- ^ Constructors
  , fit
  , fit'
  -- ^ Reexports
  , module KMeans.Data
  , module Control.DeepSeq
  ) where

-----------------------------------------------------------
-----------------------------------------------------------

import qualified Control.Monad.ST    as ST
import           Data.Coerce
import qualified Data.Maybe          as Maybe
import           Data.Vector         (Vector)
import qualified Data.Vector         as Vector
import qualified Data.Vector.Mutable as MVector
import           GHC.TypeLits
import           Positive            (Positive (..))
import qualified Positive
import           KMeans.Data
import           Control.DeepSeq
import           GHC.Generics (Generic)

-----------------------------------------------------------
-----------------------------------------------------------

-- | KMeans DS
data KMeans = KMeans
    { -- points   :: [Point]        -- ^ Points to cluster.
      clusters :: Vector Cluster -- ^ Each cluster is identified by its position in the array.
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Given n samples and the number of clusters,
-- computes for each sample its corresponding cluster.
-- The centroid of a cluster is the representative of the cluster.
fit :: [Point] -> Int -> KMeans
fit points = fit' (Vector.fromList points)

fit' :: Vector Point -> Int -> KMeans
fit' points k =
  let it = 10 :: Int -- TODO: this should iterate until no centroid has changed since last it.
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
{-# SCC recomputeClusters #-}

-- | Returns the id/index of the nearest cluster
nearest :: Point -> Vector Centroid -> Int
nearest p centroids = Vector.minIndex $ fmap (\(Centroid c) -> euclideanDistance p c) centroids
{-# SCC nearest #-}

-- | Compute k clusters from the given points and indexes.
computeClusters :: Int -> Vector (Positive, Point) -> Vector Cluster
computeClusters k points = ST.runST $ do
  mvec <- MVector.replicate k []
  Vector.foldM (\_ (Positive i, p) -> MVector.modify mvec ((:) p) i) () points
  Vector.freeze (coerce mvec)
{-# SCC computeClusters #-}

-- | Given the number of clusters and a vector of points, returns a vector of clusters.
-- Each cluster has a list of the points associated with the cluster.
randomAssignment :: Int -> Vector Point -> Vector Cluster
randomAssignment k points =
  let indexes = Positive.generatePositives (length points) . Maybe.fromJust . someNatVal . fromIntegral $ k
  in computeClusters k (Vector.zip indexes points)
{-# SCC randomAssignment #-}
