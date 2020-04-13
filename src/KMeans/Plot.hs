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
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module KMeans.Plot (
    scatter
  , elbow
  ) where

-----------------------------------------------------------

import           Data.Foldable                             (traverse_)
import qualified Data.Vector                               as Vector
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Backend
import           Graphics.Rendering.Chart.Easy
import           KMeans0                                   (Cluster (..), KMeans (..))
import qualified KMeans.Data                               as KMeans
import           Control.Monad (void)

-----------------------------------------------------------

scatter :: FilePath -> KMeans -> IO ()
scatter fp KMeans{..} = toFile fp $ do
  layout_title      .= "k-means Cluster"
  layout_background .= solidFillStyle (opaque white)

  traverse_ (\(i, cluster) -> plot (points (show i) (KMeans.toCoordinate <$> getPoints cluster)))
    $ Vector.indexed clusters


  plot $ liftEC $ do
    plot_points_title  .= "Centroids"
    plot_points_values .= centroids
    plot_points_style  .= centroidsStyle

  plots <- use layout_plots
  layout_plots .= plots ++ [annot]

    where
      centroids = Vector.toList $ fmap (KMeans.toCoordinate . KMeans.getCentroid) clusters
      centroidsStyle =  point_color        .~ opaque black
                      $ point_border_color .~ opaque black
                      $ point_border_width .~ 10.0
                      $ point_shape        .~ PointShapePlus
                      $ def

      annot = toPlot $ plot_annotation_values .~ (fmap (\(x, y) -> (x, y, "centroid")) centroids)
                     $ plot_annotation_hanchor .~ HTA_Centre
                     $ plot_annotation_vanchor .~ VTA_Top
                     $ def

-- | Elbow Plot
elbow :: FilePath -> [(Int, Double)] -> IO ()
elbow fp points' = toFile' fp $ do
  layout_title      .= "k-means Cluster"
  layout_background .= solidFillStyle (opaque white)
  layout_plots      .= [ toPlot $ plot_lines_values .~ [points']
                                $ def
                       ]
  where
    -- Inference workaround
    toFile' :: (Default a, ToRenderable a) => FilePath -> EC a () -> IO ()
    toFile' = toFile


toFile :: (ToRenderable a) => FilePath -> a -> IO ()
toFile fp = void . Backend.renderableToFile def fp . toRenderable
