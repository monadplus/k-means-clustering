import KMeans0

main :: IO ()
main = print $ force (fit' (generateStandardPoints 1000) 10)
