bestBuddy <- function( z, data, defined = rep(1, length(z)) ) {
 n <- dim(data)[[1]]
d <- dim(data)[[2]]
defined[which(defined != 1)] <- 0
defined[is.na(z)] <- 0
z[is.na(z)] <- 0

nn <- knneighbor(z, 1, data, defined)
bb <- nn[1]


return( bb = bb)
}