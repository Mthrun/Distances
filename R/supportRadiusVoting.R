supportRadiusVoting <- function(data) {


#require("dbt.general")
#require("dbt.Statistics")
#require("dbt.pareto")
#require("dbt.classifiers")
if(nrow(data) > 1000) { 

sampleData <- splitSample(data, ((1000 / nrow(data)) *100))[[1]]
}

else {
sampleData <- data
}

distances <- as.vector(dist(sampleData, method ="euclidean"))
minDist <- min(distances)
maxDist <- max(distances)
meanDist <- meanrobust(distances)
stdDist <- stdrobust(distances) 
distances <- as.matrix(dist(sampleData, method = "euclidean", diag = TRUE, upper = TRUE))


individualSupportRadius <- prctile(distances, 0.2)

kernels <- seq(minDist,maxDist,by=((maxDist-minDist)/200))

#plot
PDEplot(individualSupportRadius,defaultAxis =FALSE,xlim=c(minDist,maxDist),title='Distanzen bei dem in jeder individuellen ParetoKugel 20% der anderen Daten sind',xlab=paste('Distanzen, range [',round(minDist,4),' .. ',round(maxDist,4),']'),ylab='mag= Gaussmodell aller Distanzen')
points(kernels,dnorm(kernels,meanDist,stdDist),col='magenta',type='l')


return(individualSupportRadius = c(individualSupportRadius))


}