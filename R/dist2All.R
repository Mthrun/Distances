dist2All <- function (x, data, defined = rep(1, length(x)), distance = "euclidean") {
  # author: Raphael Päbst
  warning('Not verified, may not work properly. Please check results')
xData <- rbind(x, data)
xData <- xData[, which( defined == 1)]
distToAll <- as.vector(dist(xData, method = distance))[1:nrow(data)]

return( distToAll = distToAll)
}