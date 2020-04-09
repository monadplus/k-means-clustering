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
module KMeans.Data (
    Point(..)
  , Cluster(..)
  , Centroid(..)
  -- ^ Util
  , euclideanDistance
  , getCentroid
  , generateRandomPoints
  ) where

-----------------------------------------------------------

import           Data.Monoid
import qualified System.IO.Unsafe  as Unsafe
import qualified System.Random.MWC as Random
import           Data.Vector         (Vector)
import           Data.Bifunctor

-----------------------------------------------------------


data Point = Point {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Ord, Show)

euclideanDistance :: Point -> Point -> Double
euclideanDistance (Point p1 p2) (Point q1 q2) =
  sqrt . sum . fmap (**2) $ [(q1-p1), (p2-q2)]

newtype Cluster = Cluster { getPoints :: [Point] }
  deriving newtype (Eq, Ord, Show)

-- | The centroid is the average of the positions in each dimension.
getCentroid :: Cluster -> Centroid
getCentroid (Cluster points) =
  let
      f (Sum p) = p / (fromIntegral (length points))
      (c1, c2) = bimap f f $ foldMap (\(Point x y) -> (Sum x, Sum y)) points
  in Centroid (Point c1 c2)

-- | Centroid of a 'Cluster'
newtype Centroid = Centroid Point
  deriving newtype (Eq, Ord, Show)

----------------------------------------------------------------

-- | Generate 'n' pseudo-random points using Marsaglia's MWC256 (also known as MWC8222) multiply-with-carry generator.
generateRandomPoints :: Int -> Vector Point
generateRandomPoints n =
  fmap (\(x1, x2) -> Point x1 x2)
    $ Unsafe.unsafePerformIO
      $ do Random.withSystemRandom . Random.asGenST
             $ \gen -> Random.uniformVector gen n
