knneighborsRD <- function(k, data, defined = rep(1, ncol(data))) {
  # author: Raphael Päbst
  warning('Not verified, may not work properly. Please check results')
nn <- matrix(0, nrow(data),k)

for (i in 1:nrow(data)) {
nn[i, ] <- knneighborRD(data[i, ], k, data, defined)
}

return(nn = nn)
}