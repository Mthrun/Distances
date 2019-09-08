TransformSimilarity2MetricDistance=function(Similarity){
  #1986Legendre_Metric_and_Euclidean_properties_of_dissimilarity_c.pdf
  #page10
  if(!is.matrix(Similarity)){
    warning('Matrix Input expected, calling as.matrix')
    Similarity=as.matrix(Similarity)
  }
  n=nrow(Similarity)
  if(any(diag(Similarity)!=1)){
    stop('Transformation ist not allowed.')
  }
  ab=range(Similarity)
  if(ab[1]<0|ab[2]>1){
    stop('Transformation ist not allowed.')
  }
  Distance=sqrt((1-Similarity))
  return(Distance)
}