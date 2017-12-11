\name{knneighborsRD}
\alias{knneighborsRD}
\title{ KNNeighbors}
\description{
returns the k nearest neighbours of every point in the data.}
\usage{
knneighborsRD(k, data, defined)
}
\arguments{
  \item{k}{ The number of neighbours to each point in the data that are sought.}
  \item{data}{ A Matrix containing the data points as row vectors.}
  \item{defined}{ A row vector containing 1 for all columns that are used. If missing, all columns are used.} 
}
\value{
\item{nn}{A matrix containing the row numbers of the k neighbours of each row in data as a row vector, sorted in ascending order.}
}
\author{ Raphael Paebst}