tfidfGene2Termdistance=function(Gene2Term){
  #Gene2Term matrix
  
  fin = rowSums(Gene2Term)
  maxind = which.max(fin)
  maxfin = max(fin)
  
  #Anzahl Terme pro Gen
  tf = rep(NaN, length(fin))
  for (i in 1:dim(Gene2Term)[1]) {
    tf[i] =  fin[i]/maxfin
  }
  

  #AnzahlDokumente=length(unique(Data[,2]))
  idf = rep(NaN, length(fin))
  #N=ncol(Data)
  for (i in 1:dim(Gene2Term)[1]) {
    idf[i] = log(1 + maxfin / fin[i])
  }
  tfidf=tf*idf

  Distance = matrix(NaN, length(idf), length(idf))
  for (i in 1:length(idf)) {
    for (j in 1:length(idf)) {
      Distance[i, j] = abs(tfidf[i] - tfidf[j])
    }
  }
  return(Distance)
  #Term=term
  #gene=document
}