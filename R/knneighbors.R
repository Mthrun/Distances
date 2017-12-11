knneighbors <- function (k, data, defined = rep(1, ncol(data))) {
  # author: Raphael Päbst
  warning('Not verified, may not work properly. Please check results')
n <- nrow(data)
data <- data[, which( defined == 1)]
nn <- matrix(0, n, k)

for (i in 1:n) {
nn[i, ] <- knneighbor(data[i, ], k, data)[[1]]
}

return(nn = nn)
}