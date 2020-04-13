{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad     (forM_)
import qualified Data.Vector       as Vector
import qualified KMeans.Easy       as KMeans
import qualified KMeans.Plot       as Plot
import qualified KMeans1           as KMeans


main :: IO ()
main = do
  let n = (10000 :: Int)
      uniformPoints  = KMeans.generateUniformPoints n
      standardPoints = KMeans.generateStandardPoints n

  forM_ (zip [1..] [uniformPoints]) $ \(i, pointsV) -> do
    KMeans.plotK (fileName Elbow i)  (Vector.toList pointsV)
    Plot.scatter (fileName KMeans i) (KMeans.easy' pointsV)

data PlotType = Elbow | KMeans

fileName :: PlotType -> Int -> String
fileName Elbow i = "elbow_" ++ show i ++ ".svg"
fileName KMeans i = "kmeans_" ++ show i ++ ".svg"
