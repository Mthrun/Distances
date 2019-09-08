WeightedEuclidean=function(Data,Weights){
  n=nrow(Data)
  cc=ncol(Data)
  if(!missing(Weights))
    if(cc!=length(Weights)) stop('error')
  
  Distance=matrix(NaN,nrow = n,ncol = n)
  for(i in 1:n){
    for(j in 1:n){
      if(i<j){
        if(missing(Weights)){
          Distance[i,j]=sum((Data[i,]-Data[j,])^2)
        }else{
          temp=0
          for(k in 1:cc){
            temp=temp+(Weights[k]*Data[i,k]-Weights[k]*Data[j,k])^2
          }
          Distance[i,j]=temp
        }
      }
        
    }
    
  }
  Distance[lower.tri(Distance,diag=FALSE)]=Distance[upper.tri(Distance,diag=FALSE)]
  diag(Distance)=0
  Distance=sqrt(Distance)
  return(Distance)
}

