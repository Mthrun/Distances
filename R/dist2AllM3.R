dist2AllM3 <- function (x, data, defined = rep(1, length(x))) {

xData <- rbind(x, data)
xData <- xData[, which(defined == 1)]
dv <- as.vector(dist(xData, method = "minkowski", p = 3))[1: nrow(data)]

return(dv = dv)
}