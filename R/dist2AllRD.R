dist2AllRD <- function(x, data, defined = rep(1, length(x))) {

defined[which(defined != 1)] <- 0
dim <- sum(defined)
dv <- relDistance2All(x, data, defined)
dv <- (rep(dim, length(dv)) - dv) / dim 

return(dv = dv)
}