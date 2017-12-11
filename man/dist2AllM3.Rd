\name{dist2AllM3}
\alias{dist2AllM3}
\title{ Distance to all, using Minkowski 3 metric}
\description{
Calculates the distances from a vector x to all rows of a matrix, using the Minkowski 3 distance measure.}
\usage{
dist2AllM3(x, data, defined)
}
\arguments{
  \item{x}{ A vector of the same length as the rows of data.}
  \item{data}{ A matrix, containing the data to be compared with x.}
  \item{defined}{ A vector of the same length as x and the rows of data, containing 1 for all columns of the matrix to be used and other values for all columns to be discarded. By default all columns are used.}
}
\value{
 \item{dv}{A vector containing the distances between x and all rows of data.}
}
\author{ Raphael Paebst}