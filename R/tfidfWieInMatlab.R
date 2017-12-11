tfidfWieInMatlab <- function(DocumentsVsTerms){
# Distances = tfitfidfWieInMatlabdf(DocumentsTermsData);
# calcluates the term frequency–inverse document frequency (tfidf)
# tfidf is normalized such that unimportant differences are < 1
#
# INPUT
# DocumentsVsTerms(1:n,1:d)    binary matrix of n couments vs d terms 
#                  
# OUTPUT
# TermFreqInvDocFreq(1:n,1:d)    contains the tfidf for all documents and terms
# TermFrequency(1:d)             = AnzTerms/MaxAnzTerms
# InverseDocumentFrequency(1:n)  = slog(1/DocumentFrequency)
# DocumentFrequency/1:n)         =  AnzDocuments/MaxAnzDocuments

# CL nach ALUs Matlab-Implementierung, 3.3.17
#require(matrixStats)

# Checke Input:
if(!is.numeric(DocumentsVsTerms)){stop('tfidfWieInMatlab: Input Matrix has to be numeric. Function stops.')}

# Input erstmal binaer machen:
DocumentsVsTerms <- (DocumentsVsTerms>0)*1

# Term frequency:
AnzTerms <- colSums(DocumentsVsTerms)
MaxAnzTerms <- max(AnzTerms)
TermFrequency <- AnzTerms/MaxAnzTerms

# Document frequency and its inverse:
AnzDocuments <- rowSums(DocumentsVsTerms)
MaxAnzDocuments <- max(AnzDocuments)
DocumentFrequency <- AnzDocuments/MaxAnzDocuments
InverseDocumentFrequency <- slog(1/DocumentFrequency)

# tfidf:
TermFreqInvDocFreq <- InverseDocumentFrequency%*%t(TermFrequency)

# normalisieren so, dass innere Distanzen < 1 werden:
Normalization <- quantile(TermFreqInvDocFreq, probs = 0.8) # das 80 Percentil als Normalisierungsfaktor
TermFreqInvDocFreq = TermFreqInvDocFreq/Normalization

return(list(TermFreqInvDocFreq= TermFreqInvDocFreq, TermFrequency = TermFrequency, InverseDocumentFrequency = InverseDocumentFrequency, DocumentFrequency = DocumentFrequency))
}# end function tfidfWieInMatlab


