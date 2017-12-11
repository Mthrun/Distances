distancesM3 <- function(data, defined = rep(1, ncol(data))) {
#require("dbt.general")
n <- nrow(data)
dd <- matrix(0, n, n)

for(i in 1:n) {
dd[,i] <- dist2AllM3(data[i, ], data, defined)
}

dv <- triuvec(dd, 1)
return(list(dd = dd, dv = dv))
}