distances <- function (data, defined = rep(1, ncol(data))) {
#require("dbt.general")
  # author: Raphael Päbst
  warning('Not verified, may not work properly. Please check results')
n <- nrow(data)
dd <- matrix(0, n, n)

for (i in 1: n) {
dd[, i] <- dist2All(data[i, ], data, defined)
}

dv <- triuvec(dd, 1)

return(list(dd = dd, dv = dv)) 
}