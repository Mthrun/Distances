\name{knneighborRD}
\alias{knneighborRD}
\title{ KNNeighbor using relative distances}
\description{
returns the k nearest neighbours of x in the data, using relative distances.
}
\usage{
knneighborRD(x, k, data, defined)
}
\arguments{
  \item{x}{ A row vector, contianing the data point, who's k nearest neighbours are sought.}
  \item{k}{ the number of nearest neighbours of x that are sought.}
  \item{data}{ A matrix containing the data as row vectors.}
  \item{defined}{ A row vector of the same length as x, containing 1 for each column that is used in the calculation. If not given, all columns are used.}
}
\value{
\item{nn}{A vector containing the row numbers of the k nearest neighbours of x in data}
}
\author{ Raphael Paebst}