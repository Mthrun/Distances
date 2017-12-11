nearest <- function (Data, i, defined) {
#MT: Korrektur fuer Standardfall

  if(missing(defined)){
    #requireRpackage('FastKNN')
    requireNamespace('FastKNN')
    nnind=FastKNN::k.nearest.neighbors(i=i, DistanceMatrix(Data), k = 1)
  }else{
       # author: Raphael Päbst
       warning('Not verified, may not work properly. Please check results')
     nnind <- knneighbor(data[i, ], 2, data, defined)[[1]]
     nnind <- nnind[2]
  }
  
  
return(nnind = nnind)
}