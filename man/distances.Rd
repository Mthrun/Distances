\name{distances}
\alias{distances}
\title{ Distances~~ }
\description{
Calculates the squared euclidean distances between all rows of a matrix.}
\usage{
distances(data, defined)
}
\arguments{
  \item{data}{ A matrix, containing data as rows.}
  \item{defined}{ A vector, containing 1 for all columns of data to be used and other values for all columns to be discarded.}
}
\value{
\item{dd}{A matrix containing the distances between the rows of data.}
\item{dv}{A vector of the upper triangular distance matrix.}
}
\author{ Raphael Paebst}