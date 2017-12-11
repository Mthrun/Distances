knneighbor <- function(x, k, data, defined = rep(1, length(x)), distance = "euclidean") {
#knneighbor(x, k, data, defined, distance)
# returns the k nearest neighbors to x in the data.
#INPUT
#   \item{x}{a row vector, who's k nearest neighbors in the data are sought. }
#   \item{k}{ An integer, giving the number of nearest neighbors to x, that are sought.}
#   \item{data}{ A matrix, containing the data as row vectors.}
#   \item{defined}{ A row vector of the same length as x, containing 1 for all columns of the data that are used for the calculation. If defined is missing, all columns are used.}
#   \item{distance}{ the distance measure that is used, e.g. euclidean, minkovsky... If none is given, euclidean is the default}
#Output
#   \item{nNInd}{  a vector, containing the indices of the k rows of data that are the k nearest neighbours of x.}
#   \item{nNData}{A matrix, containing the k nearest neighbours of x as row vectors }
#   \item{nNDists}{A vector containing the distances of x to the corresponding k nearest neighbours.}
  
  warning('Not verified, may not work properly. Please check results')
  dv <- dist2All(x, data, defined, distance)
  sorting <- sort(na.last=T,dv, index = TRUE)
  dists <- sorting[[1]]
  sortInd <- sorting[[2]]
  nNInd <- sortInd[1:k]
  nNData <- data[ nNInd, ]
  nNDist <- dists[1:k]

return(list( nNInd = nNInd, nNData = nNData, nNDist = nNDist))
}