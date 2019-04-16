DistanceMatrix = function(X,method='euclidean',dim=2,outputisvector=FALSE){
  # Dmatrix = DistanceMatrix(X,distance)
  # computes the distance between objects in the data matrix, X, 
  # using the method specified by distance, which can be any of the following character strings
  #
  # INPUT
  # X[d,n]         Daten bestehend aus d Datensaetzen/Werten/Zeilen von n Vektoren/Variablen/Spalten ohne NaN
  #                Distanz wird jeweils zwischen zwei Zeilen berechnet
  #           
  # Optional         
  # method          method specified by distance string: 
  #                 'euclidean','sqEuclidean','mahalanobis','pearson','cityblock=manhatten','cosine','chebychev'=max(abs(x-y)),'jaccard','minkowski','manhattan','binary', 'canberra'=sum abs(x-y)/sum(abs(x)-abs(y)), 'maximum', 'braycur'=sum abs(x -y)/abs(x+y), "cosine"
  # dim             if method="minkowski", choose p
  # outputisvector  Falls Vector der Distanzen benoetigt wird, wird Paket pracma fuer squareform geladen
  #
  # OUTPUT
  # Dmatrix       Distance-Matrix: Pairwise distance between pairs of objects oder Vektor(outputisvector=TRUE)

  # Autor: MT
  # 1. Editor:
  #
  # EXAMPLE
  # 
  # Nota  
  # needs package stats, vegan, sphet, biotools 
  if (!is.matrix(X)) {
    X = as.matrix(X)
    warning('Data was not in matrix datatype!')
  }
  if (any(is.nan(X), na.rm = TRUE))
    stop('NaNs in Data found!')
  
  m = nrow(X) #Zeilenanzahl
  n = ncol(X) #Spaltenanzahl
  if (m < n) {
    #warning('Pruefe Anzahl von Spalten und Zeilen der Input-Matrix!')
    #warning('Vektorraum unterbestimmt: Definition eines gueltigen Distanzmasses zweifelhaft, weil jede Variable, also jeder Vektor, zu Wenig Werte also Zeilen besitzt!')
  }
  switch(
    method,
    #euclidean = {
    #  Dmatrix = as.matrix(dist(X, method = "euclidean", p = dim))
   # },
    sqEuclidean = {
      Dmatrix = (as.matrix(dist(X, method = "euclidean", p = dim))) ^ 2
    },
    binary = {
      Dmatrix = as.matrix(dist(X, method = "binary"))
    },
    braycur = {
      #requireRpackage('sphet')
	  requireNamespace('sphet')
      Dmatrix = sphet::distance(
        coord = X,
        measure = "braycur",
        output = FALSE,
        type = "distance"
      )
    },
   # cityblock = {
     # Dmatrix = as.matrix(dist(X, method = "manhattan"))
   # },
    pearson = {
      #Dmatrix = as.matrix(arules::dissimilarity(X, method = "pearson"))
      Dmatrix=1-cor(t(X),method = 'pearson')
    },
   # maximum = {
     # Dmatrix = as.matrix(dist(X, method = "maximum"))
   # },
   # canberra = {
     # Dmatrix = as.matrix(dist(X, method = "canberra"))
   # },
   fractional = {
    Dmatrix = FractionalDistance(X, dim)
   },
   fagerDissimilarity = {
     requireNamespace('parallelDist')
     DD = as.matrix(parallelDist::parDist(X, method = 'fager'))
     Dmatrix=-DD
   },
    cosine = {
      #requireRpackage("lsa")
	    #requireNamespace('lsa')
      #sim = lsa::cosine(X)
      #Dmatrix = max(sim, na.rm = T) - sim
      Dmatrix=CosinusDistance(X)
      #Dmatrix=cosine(X)
    },
    chebychev = {
      #requireRpackage('sphet')
      #Dmatrix=chebyshev(X)
	  requireNamespace('sphet')
      Dmatrix = sphet::distance(
        coord = X,
        measure = "chebyshev",
        output = FALSE,
        type = "distance"
      )
    },
   # jaccard = {
    #  Dmatrix = jaccard(X)
    #},
    #mahalanobis = {
    #  Sx = cov(X)
     # Dmatrix = Mahalanobis(X, Sx)
    #},
    minkowski = {
      #Dmatrix = as.matrix(dist(X, method = "minkowski", p = dim))
    requireNamespace('parallelDist')
    Dmatrix = as.matrix(parallelDist::parDist(X, method = method,p=dim))
    },
   # manhattan = {
   #   Dmatrix = as.matrix(dist(X, method = "manhattan"))
   # },
    {
      requireNamespace('parallelDist')
      Dmatrix = as.matrix(parallelDist::parDist(X, method = method))
      #return("Error! Bitte den String der Methodenauswahl ueberpruefen")
    }
  )
  if (!outputisvector) {
    return(Dmatrix)
  } else{
    #if(!require(pracma)){install.packages("pracma")}else{library(pracma)}  intern implementiert
    #return(squareform(Dmatrix))}
    return(Dmatrix[upper.tri(Dmatrix)])
  }
}