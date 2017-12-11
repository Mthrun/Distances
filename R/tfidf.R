tfidf=function(DocumentsTermsMatrix){
#res=tfidf(x)
  #inverste document frequency term frequency
#INPUT
  # DocumentsTermsMatrix[1:d,1:t]  d Documents and t Terms
# OUTPUT liste V with
# TermFreqInvDocFreq(1:d,1:t)    idf%+%t(tf) contains the tfidf for all documents and terms
# TermFrequency(1:t)              AnzTerms/MaxAnzTerms
# InverseDocumentFrequency(1:d)  slog(1/DocumentFrequency)
# DocumentFrequency/1:d)          AnzDocuments/MaxAnzDocuments
#
#author: MT 10/16
  documents=rowSums(DocumentsTermsMatrix)
  terms=colSums(DocumentsTermsMatrix)
  t=ncol(DocumentsTermsMatrix)
  d=nrow(DocumentsTermsMatrix)
  df=documents/max(documents)
  tf=terms/max(terms)

  # df=c() #document frequency
  # for(i in seq_len(t)){
  #   df[i]=documents[i]/max(documents)
  #   # if(!is.finite(df[i]))
  #   #   print(paste0(i,' ',documents[i]))
  # }
  # ind=c()
  # tf=c()#term frequency
  # for(i in seq_len(d)){
  #   tf[i]=terms[i]/max(terms)
  #   # if(!is.finite(terms[i])){
  #   #   ind=c(ind,i)
  #   # }
  # }
  #inverse df
  idf=slog(1/df)
  # idf[ind]=0 #log(1+1/0)=log(1+0)=log(1)=0
  
  tfidfmatrix=idf%*%t(tf)
  
   Normalization = quantile(as.vector(tfidfmatrix),0.8) # das 80 Percentil als Normalisierungsfaktor
   tfidfmatrix = tfidfmatrix/Normalization
  # 
  return(list(TermFreqInvDocFreq=tfidfmatrix,TermFrequency=tf,DocumentFrequency=df,InverseDocumentFrequency=idf))
}