nearestRD <- function( data,i, defined = rep(1, ncol(data))) {
  # author: Raphael Päbst
  warning('Not verified, may not work properly. Please check results')
defined[which(defined != 1)] <- 0
nn <- knneighborRD(data[i, ], 2, data, defined)
bb <- nn[2]

return(bb = bb)
}