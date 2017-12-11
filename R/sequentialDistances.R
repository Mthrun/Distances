sequentialDistances <- function(data, distance = "euclidean") {

seqDist <- rep(0, nrow(data))

for( i in 1 : (nrow(data) -1)) {
stupidHelpVariableForDistanceCalculation <- rbind(data[i, ], data[i+1, ])
seqDist[i] <- dist(stupidHelpVariableForDistanceCalculation, method = distance)
}
stupidHelpVariableForDistanceCalculation <- rbind(data[nrow(data), ], data[1,])
seqDist[nrow(data)] <- dist(stupidHelpVariableForDistanceCalculation, method = distance)

return(seqDist = seqDist)
}