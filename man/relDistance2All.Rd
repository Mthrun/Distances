\name{relDistance2All}
\alias{relDistance2All}
\title{ RelativeDistance2All }
\description{
Calculates the relative distaces between a data point and the rest of the data. }
\usage{
relDistance2All(x, data, defined)
}
\arguments{
  \item{x}{ A row vector, containing a data point.}
  \item{data}{ A matrix containing data points as row vectors.}
  \item{defined}{ A row vector containing 1 for all columns of the data to be used. If missing, all columns are used.}
}
\value{
\item{dv}{A vector containing the relative distances between x and all rows of data.}
}
\author{ Raphael Paebst}