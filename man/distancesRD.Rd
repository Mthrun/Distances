\name{distancesRD}
\alias{distancesRD}
\title{ Relatie Distances}
\description{
Calculates the relative distances between the rows of a matrix.}
\usage{
distancesRD(data, defined)
}
\arguments{
  \item{data}{ A matrix, containing the data as rows.}
  \item{defined}{ A vector, containing 1 for all columns of data to be used and other values for all columns to be discarded.}
}
\value{
\item{dd}{A matrix, containing the distances between the rows of data.}
\item{dv}{A vector containing the distances, Built from the upper triangular distance matrix.}
}
\author{ Raphael Paebst}