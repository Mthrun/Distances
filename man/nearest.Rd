\name{nearest}
\alias{nearest}
\title{ Nearest}
\description{
returns the index of the nearest neighbour of a given data point.}
\usage{
nearest(Data, i, defined)
}
\arguments{
  \item{Data}{ A matrix holding n data points as row vectors.}
  \item{i}{ the index of the data point, who's nearest neighbour is sought.}
  \item{defined}{ A row vector with 1 for all columns of data that are used for the computation. If missing, all columns are used.}
}
\value{
\item{nNInd}{The index of the nearest neighbour of data[i, ] }
}
\author{ Michael Thrun, Raphael Paebst}