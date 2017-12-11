\name{relDistances}
\alias{relDistances}
\title{ Relative Distances }
\description{
Calculates the relative distances between all rows of a matrix containing data.}
\usage{
relDistances(data, defined)
}
\arguments{
  \item{data}{ A matrix containing the data points as row vectors. }
  \item{defined}{ A row vector containing 1 for all columns of data to be used. If missing, all columns are used.}
}
\value{
\item{dd}{A matrix containing the relative distances between a row of data and all other rows in the corresponding row.}
\item{dv}{An upper triangular matrix containing the relative distances between the rows of data.}
}
\author{ Raphael Paebst}