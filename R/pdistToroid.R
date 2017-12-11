pdistToroid= function(X,Y,Lines,Columns,Points){
#  ToroidDist = pdistToroid(XY,Lines,Columns);
#  Calculate toroid Euclidean Distances
# 
#  INPUT
#  X(1:AnzData,1)          vector of grid koordinated on a planar grid of size Lines
#  Y(1:AnzData,1)          vector of grid koordinated on a planar grid of size Columns]
#  Lines,Columns            Size of planar grid, vectors
#  OUTPUT
#  ToroidDist(1:AnzData,1:AnzData);      distance(s) between row(s) of x and y in MATHLBs pdist form
# 

# Author: MT 04/2014
# 1.Editor: MT 01/2016
if(!missing(Points)){
X=Points[,1]
Y=Points[,2]
}
if(length(X)==length(Y)){
  AnzData=length(X)
}else{
  stop('Length of BMU positions in X and Y direction is not the same')
} 

ToroidDist= matrix(0,AnzData,AnzData)
  for(i in 1:AnzData){
    for(j in 1:AnzData){
        Ax = X[i]
        Ay = Y[i]
        Bx = X[j]
        By = Y[j]
        Dx = abs(Ax-Bx)
        Dx=pmin(Dx,(Lines-Dx+1)) #matlab: min(Dx,Lines-Dx+1);
        Dy = abs(Ay-By)
       Dy=pmin(Dy,(Columns-Dy+1)) #matlab: min(Dy,Columns-Dy+1);
        ToroidDist[i,j] = sqrt(Dx**2+Dy**2)
     } # for i
 }# for j

# PlotPixMatrix(ToroidDist)
return(ToroidDist)
}

