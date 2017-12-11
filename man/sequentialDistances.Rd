\name{sequentialDistances}
\alias{sequentialDistances}
\title{ SequentialDistances}
\description{
Returns a vector containing in a row the distances of consecutive rows of data.
}
\usage{
sequentialDistances(data, distance)
}
\arguments{
  \item{data}{ A matrix containing the data as row vectors.}
  \item{distance}{ The distance measure to be used, default is euclidean}
}
\value{
\item{seqDist}{A vector containing the distances between rows of data. seqDist[1] is the distance between data[1,] and data[2,] }
}
\author{ Raphael Paebst}