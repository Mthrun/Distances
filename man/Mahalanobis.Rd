\name{Mahalanobis}
\alias{Mahalanobis}

\title{ Pairwise Squared Generalized Mahalanobis Distances }

\description{
Function to calculate the squared generalized Mahalanobis distance between all pairs of rows
in a data frame with respect to a covariance matrix. The element of the \emph{i}-th row and
\emph{j}-th column of the distance matrix is defined as
\deqn{D_{ij}^2 = (\bold{x}_i - \bold{x}_j)' \bold{\Sigma}^{-1} (\bold{x}_i - \bold{x}_j)}
}

\usage{
Mahalanobis(X, cov, inverted = FALSE)
}

\arguments{
  \item{data}{ a matrix of data (\emph{n x d}) n cases, d variables}
  \item{cov}{ a variance-covariance matrix (\emph{p x p}).}
  \item{inverted}{ logical. If \code{FALSE} (default), \code{cov} is supposed to be a variance-covariance matrix.}
}

\value{
Distances[1:n,1:n]
}

\references{
Mahalanobis, P. C. (1936) On the generalized distance in statistics.
   \emph{Proceedings of The National Institute of Sciences of India}, 12:49-55.
}

\author{
Anderson Rodrigo da Silva <anderson.agro@hotmail.com>
}

\seealso{
\code{\link{dist}}
}
\note{
copy of function in biotools package, because this packages doesnt work under mac os
}
\examples{
# Manly (2004, p.65-66)
x1 <- c(131.37, 132.37, 134.47, 135.50, 136.17)
x2 <- c(133.60, 132.70, 133.80, 132.30, 130.33)
x3 <- c(99.17, 99.07, 96.03, 94.53, 93.50)
x4 <- c(50.53, 50.23, 50.57, 51.97, 51.37)
x <- cbind(x1, x2, x3, x4)
Cov <- matrix(c(21.112,0.038,0.078,2.01, 0.038,23.486,5.2,2.844,
	0.078,5.2,24.18,1.134, 2.01,2.844,1.134,10.154), 4, 4)
Mahalanobis(x, Cov)

# End (not run)
}
