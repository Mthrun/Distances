jaccard<-function(X){
  # D=jaccard(X)
  # The function computes dissimilarity indices Jaccard, which 
  # index is computed as 2B/(1+B), where B is Bray-Curtis dissimilarity
  #
  # INPUT

  # D      Distance Matrix
  # Autor: MT
  # 1. Editor:
  #
  # EXAMPLE
  # 
  # Nota
 # if(!require(vegan)){
  #install.packages("vegan")
  #library(vegan)}
  requireNamespace('vegan')
  djacc=as.matrix(vegan::vegdist(X, method="jaccard"))
  return(djacc)
                        
}