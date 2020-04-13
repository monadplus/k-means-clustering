{-# LANGUAGE BangPatterns               #-}
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
module KMeans1 (
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

import           Control.DeepSeq
import qualified Control.Monad.ST    as ST
import           Data.Coerce
import qualified Data.Maybe          as Maybe
import           Data.Vector         (Vector)
import qualified Data.Vector         as Vector
import qualified Data.Vector.Mutable as MVector
import           GHC.Generics        (Generic)
import           GHC.TypeLits
import           KMeans.Data
import           Positive            (Positive (..))
import qualified Positive

-----------------------------------------------------------
-----------------------------------------------------------

-- TODO Unboxed Vectors ? You need to change Point
newtype KMeans = KMeans { clusters :: Vector Cluster }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

fit :: [Point] -> Int -> KMeans
fit points = fit' (Vector.fromList points)
{-# INLINEABLE fit #-}

fit' :: Vector Point -> Int -> KMeans
fit' points k =
  let it = 10 :: Int
      initial = randomAssignment k points
  in KMeans (go it initial)
    where
      go !n clusters
        | n == 0    = clusters
        | otherwise =
            let clusters' = recomputeClusters points clusters
            in go (n-1) (force clusters')
{-# INLINEABLE fit' #-}

-- Not worth because of the vector/list:
--   let foldl' (\(!len, cs) c -> (len + 1, getCentroid c : cs)) (0, []) clusters
recomputeClusters :: Vector Point -> Vector Cluster -> Vector Cluster
recomputeClusters points clusters =
  let !centroids = fmap getCentroid clusters
      !points' = fmap (\p -> (Positive (nearest p centroids), p)) points
      !n = length clusters
  in constructCluster n points'
{-# INLINEABLE recomputeClusters #-}

-- Prob. the fastest
-- euclidean distance can be computed faster by using low level c code but not sure if it is worth the FFI call.
-- https://stackoverflow.com/questions/32150038/fastest-way-to-calculate-euclidean-distance-in-c
nearest :: Point -> Vector Centroid -> Int
nearest !p centroids =
  Vector.minIndex $ fmap (\(Centroid !c) -> notSqrtEuclideanDistance p c) centroids
{-# INLINEABLE nearest #-}

-- Lists are not an issue because they are being used in the most optimal way i.e. append-only.
constructCluster :: Int -> Vector (Positive, Point) -> Vector Cluster
constructCluster !k points = ST.runST $ do
  mvec <- MVector.replicate k []
  Vector.foldM (\_ (Positive i, p) -> MVector.modify mvec ((:) p) i) () points -- note, modify may be slow.
  Vector.freeze (coerce mvec)
{-# INLINEABLE constructCluster #-}

randomAssignment :: Int -> Vector Point -> Vector Cluster
randomAssignment !k points =
  let indexes = Positive.generatePositives (length points)
              . Maybe.fromJust
              . someNatVal
              . fromIntegral
              $ k
  in constructCluster k (Vector.zip indexes points)
{-# INLINEABLE randomAssignment #-}
