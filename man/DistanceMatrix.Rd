\name{DistanceMatrix}
\alias{DistanceMatrix}
\alias{pdist}
\title{Pairwise distance between pairs of objects}
\description{
computes the distance between objects in the data matrix, X, using the method
specified by method
}
\usage{
DistanceMatrix(X, method, dim, outputisvector)
}
\arguments{
  \item{X}{data matrix [1:n,1:d], n cases d variables}
  \item{method}{Optional, method specified by distance string: 
                'euclidean','sqEuclidean','binary','cityblock',
								'maximum','canberra','cosine','chebychev','jaccard',
								'mahalanobis','minkowski','manhattan','braycur','cosine'
	}
  \item{dim}{Optional: Dimension of X. If method="minkowski", choose p}
  \item{outputisvector}{Optional: should the output be converted to a vector}
}
\details{

'binary'
(aka asymmetric binary): The vectors are regarded as binary bits,
 so non-zero elements are 'on' and zero elements are 'off'. 
 The distance is the proportion of bits in which only one is 
 on amongst those in which at least one is on.


'cityblock'==manhattan

'maximum': Maximum distance between two components of x and y (supremum norm)

'cosine'
calculates a similarity matrix sim between all column vectors of a matrix x. 
This matrix might be a document-term matrix, so columns would be expected
 to be documents and rows to be terms. 
the distances is than defined with D=max(sim)-sim

'jaccard'
Jaccard index is computed as 2B/(1+B), where B is Bray-Curtis dissimilarity:
the number of items which occur in both elements divided by the total 
number of items in the elements (Sneath, 1957). This measure is often 
also called: binary, asymmetric binary, etc.

'mahalanobis'
 the squared generalized Mahalanobis distance between all pairs of 
 rows in a data frame with respect to a covariance matrix. 
 The element of the i-th row and j-th column of the distance matrix is defined as
D_{ij}^2 = (\bold{x}_i - \bold{x}_j)' \bold{S}^{-1} (\bold{x}_i - \bold{x}_j)


'minkowski':The p norm, the pth root of the sum of the pth powers
 of the differences of the components.

'manhattan': Absolute distance between the two vectors (1 norm aka L_1).

'chebychev'=max(abs(x-y)),

'canberra'=sum abs(x-y)/sum(abs(x)-abs(y)), 
Terms with zero numerator and denominator are omitted from the sum 
and treated as if the values were missing.
This is intended for non-negative values (e.g., counts): taking 
the absolute value of the denominator is a 1998 R modification to 
avoid negative distances.

'braycur'=sum abs(x -y)/abs(x+y)

'pearson'
1 - r if r>1 and 1 otherwise. r is Pearson's correlation coefficient.

'cosine'
s. wiki fuer simuilarity umrchnung: max(S)-S(i,j)
}
\value{
	\item{Dmatrix}{[1:n,1:n] Distance Marix: Pairwise distance
	between pairs of objects}
}
\references{
Sneath, P. H. A. (1957) Some thoughts on bacterial classification. Journal of General Microbiology 17, pages 184-200. 

Leydesdorff, L. (2005) Similarity Measures, Author Cocitation Analysis,and Information Theory. In: JASIST 56(7), pp.769-772.

Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.

Mardia, K. V., Kent, J. T. and Bibby, J. M. (1979) Multivariate Analysis. Academic Press.

Borg, I. and Groenen, P. (1997) Modern Multidimensional Scaling. Theory and Applications. Springer.

Mahalanobis, P. C. (1936) On the generalized distance in statistics. Proceedings of The National Institute of Sciences of India, 12:49-55.
}
\author{Michael Thrun}
\examples{
	example %- shown and run
	\dontrun{Dmarix = DistanceMatrix(X,method=euclidean)} %- only shown
	%\dontshown{example} %- only run
}