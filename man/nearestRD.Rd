\name{nearestRD}
\alias{nearestRD}
\title{ Nearest Relative Distance}
\description{
Returns the index of the nearest neighbour of a given point of the data, using relative distances.}
\usage{
nearestRD(data, i, defined)
}
\arguments{
  \item{data}{ A matrix containing n data points as row vectors.}
  \item{i}{ The index (row number) of the data point who's nearest neighbour is sought.}
  \item{defined}{ A row vector with 1 for all columns of the data to be used. If missing, all columns are used.}
}
\value{
\item{nNInd}{The index of the nearest neighbour of data[i,] }
}
\author{ Raphael Paebst}