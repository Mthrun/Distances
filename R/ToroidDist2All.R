ToroidDist2All=function(positionxy,AllPositions,Lines,Columns){
# Dist2All = ToroidDist2All(positionxy,AllPositions,Lines,Columns);
# Calculate toroid Euclidean Distances
#
# INPUT
# positionxy                                    One datapoint
# AllPositions(1:AnzData:2)              All Other dataPoints
# Lines,Columns                         Size of planar grid
# OUTPUT
# Dist2All(1:AnzData,1:AnzData);      distance(s) between XY and AllPositions
# author: MT

Dx = abs(AllPositions[,1]-positionxy[1])
Dy = abs(AllPositions[,2]-positionxy[2])
##  40mal Schneller als apply, kostet dennoch 68% der Laufzeit dieser Funktion
# pmin ist schneller, wenn es vorher berechnete Variablen bekommt
grx=(Lines-Dx+1)
gry=(Columns-Dy+1)
Dx = pmin(Dx,grx)
Dy = pmin(Dy,gry)
##----------------------------------
Dist2All  = sqrt(Dx**2+Dy**2)

return(Dist2All)
}