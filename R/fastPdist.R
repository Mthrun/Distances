fastPdist <- function(X){
  # res <- fastPdist(X)
  # calculates pairwise euclidean distances
  # INPUT
  # X[1:n,1:m]      data to calculate distances to
  # OUTPUT
  # dist[1:n,1:n]   distances
  
  # Quelle: http://blog.felixriedel.com/2013/05/pairwise-distances-in-r/
  #requireRpackage("RcppArmadillo")
  
  #path=paste0(SubversionDirectory(),'PUB/dbt/Dist/src/')
  #suppressWarnings(sourceCpp(paste0(path,"fastPdistC.cpp")))
  
  fastPdistC(X,X)
}