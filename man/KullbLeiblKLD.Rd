\name{KullbLeiblKLD}
\alias{KullbLeiblKLD}

\title{
 Kullback-Leibler divergence
}
\description{
 Kullback-Leibler divergence KLD of discrete distributions
}
\usage{
KullbLeiblKLD(P,Q,sym=1)
}

\arguments{
  \item{P}{two distributions having the same dirccrete range unique([P,Q])}
   \item{Q}{two distributions having the same dirccrete range unique([P,Q])}
    \item{sym}{default ==1: if sym ==1 then the symmetical KLD  is returned}
}
\details{
NaN in the distributions are ignored. not defined for zero probabilities, replace with very small values,
}
\value{
  \item{KLD}{Kullback-Leibler divergence}
  \item{p}{frequency counts of distributions}
   \item{q}{frequency counts of distributions}
    \item{x}{the unique discrete x values of P and Q: x = unique([P;Q])}
}
\author{
Michael Thrun
}
\note{
reimplemented from matlab
}



\keyword{KLD}% use one of  RShowDoc("KEYWORDS")
\keyword{ Kullback-Leibler divergence}% __ONLY ONE__ keyword per line
\keyword{ Kullback-Leibler}% __ONLY ONE__ keyword per line