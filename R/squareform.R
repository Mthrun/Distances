squareform=function(X)
# y=squareform(x)
# analog zu matlab übernommen aus Rpaket
# Format or generate a distance matrix.
#
# INPUT
# X         numeric vector or matrix
#
# OUTPUT
# y      Returns a matrix if x is a vector, and a vextor if x is a matrix.

# Autor: [Package pracma version 1.6.4 Index], kopiert von MT
#
# Example:
# x <- 1:6
# y <- squareform(x)
# #  0  1  2  3
# #  1  0  4  5
# #  2  4  0  6
# #  3  5  6  0
# all(squareform(y) == x)
# # TRUE
{
requireNamespace('pracma')
  if(is.matrix(X)){
    if(isSymmetric(X)){
      return(X[upper.tri(X)])
    }else{
      return(pracma::squareform(X))
    }
  }else{
    return(pracma::squareform(X))
  }
}