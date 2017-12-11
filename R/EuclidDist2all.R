EuclidDist2all=function(x,Data){
# Dist2x = EuclidDist2all(x,Data);
# Euclidic distance of x to all rows in Data
#
# INPUT
# x(1:d)           row vector of length d
# Data(1:n,1:d)    matrix of size nxd 
# OPTIONAL
# OUTPUT
# DistToAll(1:n)   column vector with distances from x to all rows in Data

# MT 2015 reimplemented from ALUs matlab version

AnzData=nrow(Data)
X = ones(AnzData,1)*x         # Matrix wie Daten gefuellt        mit  x
Dist2x = rowSums(((Data-X)^2)) # Summe der quadrierte Differenzen  zu  x
DistToAll = sqrt(Dist2x)         # Euclid-sche Distanzen der Daten    zu x

return(DistToAll)
}