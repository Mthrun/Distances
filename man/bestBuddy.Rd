\name{bestbuddy}
\alias{bestBuddy}
\title{  Bestbuddy}
\description{
Returns the bestbuddy of a given data point e.g. the data point closest to it.
}
\usage{
bestBuddy(z, data, defined)
}
\arguments{
  \item{z}{ a vector of the same length as the rows of data. A data point to be compared to the rest.}
  \item{data}{ A matrix containing all points of data to be compared to z.}
  \item{defined}{ A vector of the same length as z and the rows of data, containing 1 for all columns of data to be used and other values for all columns to be discarded.}
}
\value{
item{bb}{ the index of the bestbuddy to z.}]
 }
\author{ Raphael Paebst}