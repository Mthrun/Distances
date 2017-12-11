\name{distancesneighbors}
\alias{distancesneighbors}
\title{ Neighbours}
\description{
Returns all neighbours for all data points in ascending order}
\usage{
distancesneighbors(data, nr, defined)
}
\arguments{
  \item{data}{ A matrix containing the data points as row vectors}
  \item{nr}{ a vector containing row names. If missing, row numbers are used.}
  \item{defined}{ A row vector containing 1 for all columns of data that are used. If missing, all columns are used.}
}
\value{
\item{nn}{ A matrix containing all neighbours of all data points in the corresponding g row, in ascending order.}
}
\author{ Raphael Paebst}