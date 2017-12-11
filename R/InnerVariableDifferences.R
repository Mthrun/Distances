InnerVariableDifferences = function (Variable){
# all inner differences di = xi-yi for all xi,yi in Variable
#
# INPUT
# Variable(1:n)    matrix of size nxd without NaNs in columns where defined = 1
#
# OUTPUT
# Di     matrix of all inner differences
# mt 
# zurueckrechnen auf unquadrierte Differenzen
AnzData = length(Variable) 
Di =fastPdist(as.matrix(Variable))
for(i in 1:AnzData){
for(j in 1:AnzData){
if(Variable[i] < Variable[j]){
Di[i,j] = -Di[i,j] 
}  # if
}  #for j
}  #for i
return(Di)
}