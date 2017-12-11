\name{tfidfdistance}
\alias{tfidfdistance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
tfidfdistance
}
\description{
Distance measure between Genes based on GeneOntology
}
\usage{
tfidfdistance(NCBIs,GeneOntologyPath = GOdataDi('09Originale'), LRNfilename = 'GOAdirekt.lrn',Silent=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{NCBIs}{ [1:n]
   NCBI numbers, see \code{ORA} documentation
}
  \item{GeneOntologyPath}{
  OPTIONAL, path of GeneOntology data base, see \code{ORA} documentation
}
  \item{LRNfilename}{
  OPTIONAL, pfilename of GeneOntology data base, see \code{ORA} documentation
}
  \item{Silent}{
  OPTIONAL, If =FALSE: print stepweis computations
}
}
\details{
By using the inverse document frequency (idf) [Sparck Jones, 1972] a distance between these genes is defined.
If all n Genes are manually annotated in the Geneontology, the distance matrix is a nxn matrix, in the other case smaller.

Details in [Thrun, 2016]
}
\value{
List V with

 \item{Distance}{[1:m,1:m]                 distancematrix, Only Between Genes which are manually curated in Gene Ontology: m=<n
 }
 \item{Data}{[1:m,1:k]                     Correct MDS transformation of Distance to Points
 }
 \item{Gene2TermP}{[1:m,1:l]              Genes to GOterms matrix, see ORA documentation
 }
  \item{NCBIsFound}{[1:m]                  NCBI numbers found in GeneOntology, these genes are used in the distance matrix
  }
 AnnotedInFollowingGOterms{[1:l]    GoTerms were the Genes were manually annotated. 
 }
}
\references{
Sparck Jones, K.: A statistical interpretation of term specificity and its application in retrieval, Journal of documentation, Vol. 28(1), pp. 11-21. 1972.

Thrun, M.C., A System for Projection Based Clustering through Self-Organization and Swarm Intelligence, (Doctoral dissertation), Philipps-Universität Marburg, Germany, 2016.

}
\author{
Michael Thrun
}

\examples{
# path2=ReDi('ChronificationGenes2016/09Originale')
# name2='SchmerzGene2016Jan535.names'
# NamesV=ReadNAMES(name2,path2)
# NCBI=NamesV$Key
# V=tfidfdistance(NCBI)
}


\seealso{
\code{ORA} package
}
