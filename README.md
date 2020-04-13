## K-Means Clustering

`k-means clustering` algorithm aims to partition `n` observations into `k` clusters in which each observation belongs to the cluster with the `nearest mean` (cluster centers or cluster centroid), serving as a prototype of the cluster. This results in a partitioning of the data space into `Voronoi cells`. It is popular for cluster analysis in data mining.

The total cost on d-dimensions is `O(t*n*k*d)-time`, where t = #iterations until convergence of clusters, n = #points, k = #clusters.

![Figure 1. k-means Clustering Plot](./img/kmeans.png)

## Future Work

Implement a faster k-means Clustering Algorithm: <https://annals-csis.org/proceedings/2014/pliks/258.pdf>
