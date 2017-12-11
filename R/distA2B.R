distA2B <- function(a, b, defined = rep(1, ncol(a))) {

a <- a[, which(defined == 1)]
b <- b[, which(defined == 1)]
a2b <- matrix(0, nrow(a), nrow(b))

for (i in 1: nrow(a)) {
dv <- dist2All(a[i, ], b, defined)
a2b[i, ] <- dv
}

return( a2b = a2b)
}