\name{CosinusDistance}
\alias{CosinusDistance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kosinus Distanz
}
\description{
Berechnet die Kosinus Distanz eines Datensatzes
}
\usage{
CosinusDistance(Data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n,1:d] n cases, d variales,  Daten
}
}
\details{
 \url{https://en.wikipedia.org/wiki/Cosine_similarity}
}
\value{
Distance          [1:n,1:n] Matrix mit Distanzen
}
\references{
\url{http://stats.stackexchange.com/questions/31565/is-there-an-r-function-that-will-compute-the-cosine-dissimilarity-matrix}
}
\author{
MT
}
\note{
Umerechnung von Similarity zu Distance nach VL
}