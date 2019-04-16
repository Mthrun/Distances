FractionalDistance=function(Data,p){
  #file:///C:/Subversion/PRO/Research/WissenAusDaten2014/70DistanceDistributions/06OtherPublications/2001Aggarwal_On_the_Surprising_Behavior_of_Distance_Metric_in_High_Dimensional_Space.pdf
  n=nrow(Data)
  Distance=matrix(0,n,n)
  for(i in 1:n){
    for(j in 1:n){
      #if(i<J){
        Distance[i,j]=sum((Data[i,]-Data[j,])^p)^(1/p)
      #}
    }
  }
  return(Distance)
}