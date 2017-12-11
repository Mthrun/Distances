\name{dist2AllRD}
\alias{dist2AllRD}
\title{ relative distance to all.}
\description{
Calculates the relative distances from a vector x to all rows of a matrix data. }
\usage{
dist2AllRD(x, data, defined)
}
\arguments{
  \item{x}{ A vector to be compared to the rows of data.}
  \item{data}{ A matrix containing the data to be compared with x as rows.}
  \item{defined}{ A vector of the same length as x and the rows of data, containing 1 for all columns of the matrix to be used and other values for all columns to be discarded. By default all columns are used.}
}
\value{
\item{dv}{A Vector, containing the distances between x and the rows of data.}}
\author{ Raphael Paebst}