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
module Positive (
    Positive(..)
  , toPositive
  , generatePositives
  , generatePositives'
  ) where

------------------------------------------------------------------

import           Data.Proxy
import           Data.Vector       (Vector)
import           GHC.TypeLits
import qualified System.IO.Unsafe  as Unsafe
import qualified System.Random.MWC as Random

------------------------------------------------------------------

-- | Returns a positive integer x < k
-- TODO type level range
newtype Positive = Positive { getInteger :: Int }
  deriving newtype (Eq, Ord, Show)

toPositive :: (KnownNat n) => Proxy n -> Int -> Positive
toPositive proxy x = Positive $ abs (x `mod` fromIntegral (natVal proxy))

generatePositives :: Int -> SomeNat -> Vector Positive
generatePositives m (SomeNat proxy) = generatePositives' m proxy

generatePositives' :: (KnownNat n) => Int -> Proxy n -> Vector Positive
generatePositives' m proxy = fmap (toPositive proxy) $ generateInts m

generateInts :: Int -> Vector Int
generateInts m =
  Unsafe.unsafePerformIO $ do
    Random.withSystemRandom . Random.asGenST
      $ \gen -> Random.uniformVector gen m
    -- ^^^^ withSystemRandom is expensive, use create if you want to reuse RNG
