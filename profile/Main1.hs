{-# LANGUAGE NumericUnderscores #-}
import KMeans1
import           Data.Time     (diffUTCTime, getCurrentTime)
import           Text.Printf
import           Control.Exception (evaluate)

main :: IO ()
main = do
  time <- timeit (fit' (generateStandardPoints 100000) 2)
  printf "Total time: %.2fs\n" time

timeit :: (NFData a) => a -> IO Double
timeit a = do
   t0 <- getCurrentTime
   evaluate (rnf a)
   t1 <- getCurrentTime
   return $ realToFrac (t1 `diffUTCTime` t0)
