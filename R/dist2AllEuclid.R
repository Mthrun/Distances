dist2AllEuclid <- function (x, A) {
# Liefert die euklidschen Distanzen eines Vektors x zu allen Zeilen einer Matrix "A"

# V <- dist2AllEuklid(x,A)

# Input:
# x		Vector [1:n]; numeric;
# A		Matrix [1:k, 1:n]; numeric;

# Output:
# Dists	Vektor [1:k]; numeric; Euclidean Distance of x to every row of A

# Autor: CL, 25.10.2017

if(!is.numeric(x)&is.numeric(A)){stop('dist2AllEuclid: x and A have to be numeric vector and matrix respectively. Function stops!')}
if(!length(x)==length(A[1,])){stop('dist2AllEuclid: x has to be of same length as rows of A, i.e. number of colums == length x. Function stops!')}

# Fuer jede Zeile der Matrix (= "apply(A, 1") wende die Funktion an, die den euklidschen Abstand von dieser Zeile zum Vektor x berechnet:
Abstaende <- apply(A, 1, function(a){sqrt(sum((a-x)^2))})

return(Dists = Abstaende)

## OLD Version:
# dist2All <- function (x, data, defined = rep(1, length(x)), distance = "euclidean") {
  # # author: Raphael Päbst
  # warning('Not verified, may not work properly. Please check results')
# xData <- rbind(x, data)
# xData <- xData[, which( defined == 1)]
# distToAll <- as.vector(dist(xData, method = distance))[1:nrow(data)] ### ULTRA langsam fuer grosse Matrizen! Es werden doch alle Distanzen berechnet und dann aber nicht ausgegeben... Unnuetz!!

# return( distToAll = distToAll)
}