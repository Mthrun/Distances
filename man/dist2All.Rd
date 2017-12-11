\name{dist2All}
\alias{dist2All}
\title{ Distances to all data points}
\description{
Calculates all distances from a given vector to the rows of a matrix.}
\usage{
dist2All(x, data, defined, distance)
}
\arguments{
  \item{x}{ A vector containing the data point to be compared to data.}
  \item{data}{ A matrix containing the data points to be compared with x.}
  \item{defined}{ A vector of the same length as x and the rows of data, containing 1 for all columns of the data to be compared and any other value for columns to be discarded.}
  \item{distance}{ A string marking, which distance measure is to be used. Euclidean by default.}
}
\value{
\item{distToAll}{A vector containing the distances from x to all rows of data.}
  }
\note{This function is very inefficient for large Data! For euclidean distance form a given vector to the rows of a matrix use dist2AllEuclid instead.}
\author{ Raphael Paebst}