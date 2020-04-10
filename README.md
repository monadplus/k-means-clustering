## k-means Clustering

Aims to partition n observations into k clusters in which each observation belongs to the cluster with the nearest mean (cluster centers or cluster centroid), serving as a prototype of the cluster. This results in a partitioning of the data space into Voronoi cells. It is popular for cluster analysis in data mining.

## Cost Analysis

n = #points
k = #clusters

getCentroid = O(n + n + ops) = O(n)
  - length = O(n)
  - centroid = O(n)

nearest = O(k)
  - min = O(k)
  - k * euclidean distance = k * O(1) = O(k)

computeCluster = n + O(1) = O(n)

recomputeCluster = O(n*k)
  - k * getCentroid = k * O(n) = O(k*n)
  - n * nearest = b * O(k) = O(n*k)
  - compute cluster = O(n+1)

generatePositives =

randomAssignment = O(n + C)
  - generate positives = O(n + C), where C = amortized cost of generating PRN
  - computeCluster = O(n)

Total cost = O(d*n*k)
  - list to vec             = O(n)
  - random assigments       = O(n+C)
  - it * recompute cluster  = O(d*n*k)
