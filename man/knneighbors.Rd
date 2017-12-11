\name{knneighbors}
\alias{knneighbors}
\title{ KNNeighbors}
\description{
Computes the k nearest neighbours to every data point.}
\usage{
knneighbors(k, data, defined)
}
\arguments{
  \item{k}{ The number of neighbours to be computed}
  \item{data}{ A matrix containing the data points as row vectors}
  \item{defined}{ A row vector, containing 1 for every column of the data to be used in the computation. If missing, all columns are used}
}
\value{
\item{nn}{A matrix with n rows and k kolumns. Each row holds the k neighbours of the corresponding data point in ascending order.}
}
\author{ Raphael Paebst}