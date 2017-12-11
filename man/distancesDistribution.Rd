\name{distancesDistribution}
\alias{distancesDistribution}
\title{ Distance Distribution}
\description{
Calculates the distribution of the distances between the data points
}
\usage{
distancesDistribution(data)
}
\arguments{
  \item{data}{ A matrix, containing data as rows.}
}
\value{
\item{cumulativeKernels}{ together with the cumulativeDistanceDensity this can be used to plot the CDF of the data}
\item{cumulativeDistanceDensity }{together with the cumulativeKernels, this can be used to plot the CDF of the data}
\item{kernels}{together with the pdeDistances, this can be used to plot the PDF of the data}
\item{pdeDistaces}{together with the kernels, this kan be used to plot the PDF of the data}  
\item{distances}{A vector, containing the distances on which the distribution estimate is calculated} 
}
\author{ Raphael Paebst}