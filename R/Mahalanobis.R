Mahalanobis=function (X, cov, inverted = FALSE) 
  # MT: kopiert aus biotools, da biotools unter mac nicht installierbar
{
  if (!inherits(X, c("data.frame", "matrix"))) 
    stop("X must be a data.frame or matrix!")
  stopifnot(is.matrix(cov))
  if (ncol(X) != ncol(cov)) 
    stop("incompatible dimensions!")
  x <- as.matrix(X)
  n <- nrow(x)
  distances <- matrix(0, n, n)
  dimnames(distances) <- list(rownames(X), rownames(X))
  if (!inverted) {
    for (i in 1:n) {
      for (j in 1:n) {
        if (i > j) 
          distances[i, j] <- crossprod((x[i, ] - x[j, ]), solve(cov, 
                                                         (x[i, ] - x[j, ])))
      }
    }
  }
  else {
    for (i in 1:n) {
      for (j in 1:n) {
        if (i > j) 
          distances[i, j] <- crossprod((x[i, ] - x[j, ]), crossprod(cov, 
                                                             (x[i, ] - x[j, ])))
      }
    }
  }
  return(as.matrix(as.dist(distances)))#Zu tun damit aus unteren diagonal matrix eine komplette matrix wird
}