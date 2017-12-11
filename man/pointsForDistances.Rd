\name{pointsForDistances}
\alias{pointsForDistances}
\title{ Points for Distances}
\description{Returns all points within a given distance. Uses either sammons mapping or traditional multidimensional scaling.}
\usage{
pointsForDistances(distances, dim, searchMethod, plotIt)
}
\arguments{
  \item{distances}{A dist object containing the distances.}
  \item{dim}{ The dimension for the scaling, if none is given, it is determined automatically}
  \item{searchMethod}{ The search method, either "standard" or "sammons" default is standard multidimensional scaling. }
  \item{plotIt}{ A boolean, indicating, if a plot should be made. Default is FALSE.}
}
\value{
\item{points}{A vector containing the points for the given distances.}
\item{MSSE}{The middle of the squared strain error}
\item{error}{The strain error.}
}
\author{ Raphael Paebst}