{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE BangPatterns               #-}
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
  -- ^ Coordinate
  , Coordinate(..)
  ) where

-----------------------------------------------------------

import           Data.Monoid
import qualified System.IO.Unsafe  as Unsafe
import qualified System.Random.MWC as Random
import           Data.Vector         (Vector)
import           Data.Bifunctor
import           Control.DeepSeq
import           GHC.Generics (Generic)

-----------------------------------------------------------

data Point = Point {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

euclideanDistance :: Point -> Point -> Double
euclideanDistance (Point p1 p2) (Point q1 q2) =
  sqrt . sum . fmap (**2) $ [(q1-p1), (p2-q2)]

newtype Cluster = Cluster { getPoints :: [Point] }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show)
  deriving anyclass (NFData)


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
--
-- Notice, this returns values in (0,1).
generateRandomPoints :: Int -> Vector Point
generateRandomPoints n =
  fmap (\(x1, x2) -> Point x1 x2)
    $ Unsafe.unsafePerformIO
      $ do Random.withSystemRandom . Random.asGenST
             $ \gen -> Random.uniformVector gen n

-------------------------------------------------------------------

class Coordinate a where
  toCoordinate :: a -> (Double, Double)

instance Coordinate Point where
  toCoordinate (Point !x!y) = (x,y)

instance Coordinate Centroid where
  toCoordinate (Centroid (Point !x !y)) = (x, y)
