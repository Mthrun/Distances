relDistances <- function (data, defined = rep(1, ncol(data))) {
#require("dbt.general")
  # author: Raphael Päbst
  warning('Not verified, may not work properly. Please check results')
data <- data[ , which(defined == 1) ]
dd <- matrix(0,nrow(data), nrow(data))

for (i in 1: nrow(data)) {
dd[ , i] <- relDistance2All( data[i, ] , data, defined)
}
 
dv <- triuvec(dd, 1)
 
return( list(dd = dd, dv = dv))
}