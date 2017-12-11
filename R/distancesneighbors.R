distancesneighbors <- function(data, nr = c(1: nrow(data)), defined = rep(1, ncol(data))) {
#author: Raphael Paebst
kn <- knneighbors( nrow(data), data)
nn <- matrix(0, nrow(data), nrow(data))

for (i in 1: nrow(data)) {
nn[, i] <- nr[kn[, i]] 
}

return(nn = nn)
}