\name{distancesCB}
\alias{distancesCB}
\title{ Distances (City Block)}
\description{
Calculates the distance between all rows of a matrix, using the City Block ( Manhattan) metric.}
\usage{
distancesCB(data, defined)
}
\arguments{
  \item{data}{ A matrix, containing data as rows.}
  \item{defined}{ A vector, containing 1 for all columns of data to be used and other values for all columns to be discarded.}
}
\value{
\item{dd}{A matrix containing the distances between the rows of data.}
\item{dv}{A vector, containing the distances, taken from the upper triangular distance matrix.}
}
\author{ Raphael Paebst}