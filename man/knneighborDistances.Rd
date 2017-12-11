\name{knneighborDistances}
\alias{knneighborDistances}
\title{ return the k indices and the k distances of the k nearest neighbors for all points  k                   number of nearest neigbors to find Distances(1:n,1:n)  matrix of distances betwenn the 1:n poits  or  Distances(1:n2,1)  == squareform(Distances(1:n,1:n)  OUTPUT NNind(1:n,1:k)         NNInd(i,:) are the indices of the k-nearest Neighbors of data point i NNdists(1:n,1:k)       distances to  the nearest neighbors    author: reimplemented from matlab by MT    }
\arguments{
 k                   number of nearest neigbors to find
 Distances(1:n,1:n)  matrix of distances betwenn the 1:n poits  or 
 Distances(1:n2,1)  == squareform(Distances(1:n,1:n)
}
\value{List with
 NNind(1:n,1:k)         NNInd(i,:) are the indices of the k-nearest Neighbors of data point i
 NNdists(1:n,1:k)       distances to  the nearest neighbors   
}
\description{
 return the k indices and the k distances of the k nearest neighbors for all points  k 
 number of nearest neigbors to find Distances(1:n,1:n)  matrix of distances betwenn
# the 1:n poits  or  Distances(1:n2,1)  == squareform(Distances(1:n,1:n)  
}
\author{Michael Thrun
}
