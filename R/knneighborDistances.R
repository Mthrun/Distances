knneighborDistances=function(k,Distances){
# [NNind , NNdist] = knneighborDistances(k,Distances);
# return the k indices and the k distances of the k nearest neighbors for all points 
# k                   number of nearest neigbors to find
# Distances(1:n,1:n)  matrix of distances betwenn the 1:n poits  or 
# Distances(1:n2,1)  == squareform(Distances(1:n,1:n)
#
# OUTPUT
# NNind(1:n,1:k)         NNInd(i,:) are the indices of the k-nearest Neighbors of data point i
# NNdists(1:n,1:k)       distances to  the nearest neighbors   
# author: reimplemented from matlab by MT 2014
#1.Editor: MT 01/2016  

  if(is(Distances)[2]=="vector"){ # vektorform
  Distances = squareform(Distances)
  }# [l,c]
  
  #SortedDists=apply(Distances, 2, sort)
  
#   S=sort(na.last=NA,VectorOfInputDists,index.return=TRUE)
    S=sortdescending(-Distances)
    SortedDists=-S$sort
    Sind=S$indices  


  NNdists = t(SortedDists[2:(k+1),])
  if(k==1)
    NNind = as.matrix(Sind[2:(k+1),])
  else
    NNind = t(Sind[2:(k+1),])
  
return(list(NNind= NNind, NNdists = NNdists))
}