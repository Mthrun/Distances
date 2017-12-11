relDistance2All <- function(x, data, defined = rep(1, ncol(data))) {
  # author: Raphael Päbst
  warning('Not verified, may not work properly. Please check results')
defined[which(defined != 1)] <- 0
e <- data - matrix(x, nrow(data) , length(x), byrow = TRUE)
e <- e^2
m <- apply(data, 2, mean)
s <- apply(data, 2, sd)
n <- m + s
n <- n^2
ra <- -e / matrix(n , nrow(data), length(n), byrow = TRUE)
ra <- exp(ra)
dv <- as.vector(ra %*% defined)

return(dv = dv)
}