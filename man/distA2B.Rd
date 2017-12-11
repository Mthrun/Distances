\name{distA2B}
\alias{distA2B}
\title{ Distances a to b}
\description{
Calculates the distances from all rows of a to b.}
\usage{
distA2B(a, b, defined = rep(1, ncol(a)))
}
\arguments{
  \item{a}{ A matrix, containing data as rows.}
  \item{b}{ Another matrix, containing data as rows.}
  \item{defined}{ A vector of the same length as the rows of a and b, containing 1 for all columns to be used and other values for all columns to be discarded.}
}
\value{
\item{a2b}{ A matrix containing the distances between the lines of a and b.}
}
\author{ Raphael Paebst}