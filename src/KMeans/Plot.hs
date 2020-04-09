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
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module KMeans.Plot (
    scatter
  , line
  ) where

-----------------------------------------------------------

import           KMeans(KMeans(..))
import qualified KMeans

-----------------------------------------------------------

--charts: https://hackage.haskell.org/package/Chart
--example: https://github.com/timbod7/haskell-chart/wiki/example-13

-- | Draws a scatter plot of the given k-means.
--
-- I like this plot https://datascienceplus.com/k-means-clustering/
scatter :: FilePath -> KMeans -> IO ()
scatter _ _ = undefined


-- | Draws a line plot of the given points.
line :: FilePath -> [Double] -> IO ()
line _ _ = undefined
