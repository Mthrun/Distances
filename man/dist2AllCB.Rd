\name{dist2AllCB}
\alias{dist2AllCB}
\title{ Distance to all, using City Block metric}
\description{
Calculates the distance from a given vector to all rows of a matrix, using the City Block distance (Manhattan) measure.}
\usage{
dist2AllCB(x, data, defined)
}
\arguments{
  \item{x}{ A vector to be compared to  data.}
  \item{data}{ A matrix, containing the data to be compared to x.}
  \item{defined}{ A vector of the same length as x and the rows of data, containing 1 for all columns of the matrix to be used and other values for all columns to be discarded. By default all columns are used.}
}
\value{
\item{dv}{A vector, containing the distances from x to the rows of data.}
}
\author{ Raphael Paebst}