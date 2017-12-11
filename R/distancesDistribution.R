distancesDistribution <- function(data) {
#require("dbt.classifiers")
#require("dbt.pareto")
  # author: Raphael Päbst
  warning('Not verified, may not work properly. Please check results')
maxNrOfPoints <- 1000
nrOfPoints <- nrow(data)

if (nrOfPoints > maxNrOfPoints) {
percentage <- (maxNrOfPoints / nrOfPoints) * 100
sampledData <- splitSample(data, percentage)[[1]]
}
else {
sampledData <- data
}

distances <- as.vector(dist(sampledData, method = "euclidean"))
paretoRadius <- ParetoRadius(distances)
pce <- PCE(distances, paretoRadius)
cumulativeKernels <- pce[[1]]
cumulativeDistanceDensity <- pce[[2]]
pdeEstimation <- ParetoDensityEstimation(distances, paretoRadius)
kernels <- pdeEstimation[[1]]
pdeDistance <- pdeEstimation[[2]]

return(list(cumulativeKernels = cumulativeKernels, cumulativeDistanceDensity = cumulativeDistanceDensity , kernels = kernels, pdeDistance = pdeDistance, distances = distances ))
}