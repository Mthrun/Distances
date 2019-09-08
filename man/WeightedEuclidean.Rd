\name{WeightedEuclidean}
\alias{WeightedEuclidean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Eulidean Distance Matrix
}
\description{
Produces Weighted Eulidean Distance Matrix
}
\usage{
WeightedEuclidean(Data, Weights)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n,1:c] data matric
}
  \item{Weights}{
vector [1:c]
}
}
\details{
The second and las example shows the difference to common Euclidean distance.
Only the first example shows how to use the weighted euclidean for cluster analysis.
}
\value{
Distance Matrix
}

\author{
Michael Thrun
}
\note{
should be implemented in Rcpp for efficiency
}

\examples{
#lets assume that feature 2 and 3
# are correlated, then one can use:
DistanceMatrix=WeightedEuclidean(FCPS$Hepta$Data,Weights = c(1,0.5,0.5))


##Comparisions for (un)correlated features:

#There is no difference to normal Euclid
# if uncorrelated, because the weights are always one
WeightedEuclidean(FCPS$Hepta$Data,Weights = c(1,1,1))-DistanceMatrix(FCPS$Hepta$Data)

#in case of correlations there is a difference
WeightedEuclidean(FCPS$Hepta$Data,Weights = c(1,0.5,0.5))-DistanceMatrix(FCPS$Hepta$Data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Euclidean }% use one of  RShowDoc("KEYWORDS")
\keyword{Weighted}% __ONLY ONE__ keyword per line
