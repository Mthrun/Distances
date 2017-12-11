CosinusDistance<-function(Data){
  # Distance=CosinusDistance(Data)
  #
  # Berechnet die Kosinus Distanz eines Datensatzes
  # INPUT: 
  # Data              [1:n,1:d] n cases, d variales,  Daten
  # OUTPUT: 
  # Distance          [1:n,1:n] Matrix mit Distanzen
  # 
  #
  # Author: MT 10/2016
  
  #Nota:
  # https://en.wikipedia.org/wiki/Cosine_similarity
  cos_hlp <- function(iData) 
  {
    A = Data[iData[1],]
    B = Data[iData[2],]
    return( sum(A*B)/(sqrt(sum(A^2))*sqrt(sum(B^2))) )
  }   
  n <- nrow(Data) 
  cmb <- expand.grid(i=1:n, j=1:n) 
  #using global variable Data
  Similarity <- matrix(apply(cmb,1,cos_hlp),n,n)
  Distance=max(Similarity)-Similarity #s. KD vl
	# wird leider nur mit einer Genauigkeit von e-16 Null in der diagonalen...
	diag(Distance)=0
  return (Distance)
}