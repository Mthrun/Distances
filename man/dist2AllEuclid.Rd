\name{dist2AllEuclid}
\alias{dist2AllEuclid}
\title{ Distances to all data points}
\description{
Calculates the euclidean distances from a given vector to all rows of a matrix.}
\usage{
dist2AllEuclid(x, A)
}
\arguments{
  \item{x}{ A vector[1:n] containing the data point to be compared to the Matrix.}
  \item{A}{ A matrix[1:k,1:n] containing the data points to be compared with x.}
}
\value{
\item{Dists}{A vector[1:k] containing the distances from x to all rows of A.}
  }
\author{ Catharina Lippmann}