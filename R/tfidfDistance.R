tfidfDistance <- function(DocumentsTermsMatrix){
# V <- tfidfDistance(DocumentsTermsMatrix)
# TfidfDistances <- V$TfidfDistances
# TfidfWeights   <- V$TfidfWeights
# TermFrequency  <- V$TermFrequency 
# InverseDocFreq <- V$InverseDocFreq 
#
# Computes the term frequency inverse document frequency (tfidf) distance
# for a DocumentsTermsMatrix.
# In case of genes with annotated GOterms from gene ontology
# genes can be interpreted as documents and GOterms as terms. 
#
# INPUT:
# DocumentsTermsMatrix[1:n, 1:m] 	DocumentsTermsMatrix[i,j] > 0 iff term j (GOterm j) 
#																	is relevant for document i (gene i).
#																	DocumentsTermsMatrix[i,j] is the frequency of
#																	term js occurance in document i.
#                     
# OUTPUT:
# List V of 4:
# TfidfDistances[1:n, 1:n]	       tdfidf distances between the documents = euclidean distance of TfidfWeights
# TfidfWeights[1:n,1:m] 		       The tfidf weights	= TermFrequency %*% InverseDocFreq
# TermFrequency[1:n,1:m] 		       Normalized term frequency for each term in each document
#													        	= #term's occurance in document/max{#any term's occurance in the document}
# InverseDocFreq[1:m]				       Inverse term frequency for each term in document collection
#														       = log(#documents/#documents containing term), where #documents = n.
# CALCULATION:
# tfidf(d_i, t_j) := tf_(d_i, t_j)*idf_(t_j), i in {1,...,n}, j in {1,...,m} with
# tf_(d_i, t_j) := f(d_i, t_j)/(max{f(d_i, t_z): z in {1,...,m}}), where
# f(d_i, t_j) := frequency of term t_j in document d_i, and
# idf(t_j) := log(n/n_j), with 
# n_j := number of documents containing term t_j. 

#  CL 2017 

require(matrixStats)
#todo ueberpruefe Input
if(!is.numeric(DocumentsTermsMatrix)){stop('tfidfDistance: Input DocumentsTermsMatrix has to be numeric. Function stops.')}

N <- dim(DocumentsTermsMatrix)[1] # Anzahl Dokumente/Gene
# M <- dim(DocumentsTermsMatrix)[2] # Anzahl Terme
BinaryDocsTermsMat <- DocumentsTermsMatrix > 0 # Matrix binaer machen
Nj <- colSums(BinaryDocsTermsMat) # Zeilensummen = zaehlen der Dokumente, die Term enthalten
Idf <- log(N/Nj) # Inverse Document Frequency
# f(di,tj) = DocumentsTermsMatrix[i,j]
MaxFreqOfTermInDoc <- matrixStats::rowMaxs(DocumentsTermsMatrix) # Maximale Auftrittshaeufigkeit eines Terms pro Dokument
Tf <- DocumentsTermsMatrix/MaxFreqOfTermInDoc # TermFrequency
Tfidf <- t(t(Tf)*Idf) # Term Frequency Inverse Document Frequency (Transponieren und ruecktransponieren, da spaltenweise Multiplikation mit Idf)
TfidfWeights <- Tfidf

# Jetzt den Abstand von jedem Dokument zu jedem anderen Dokument berechnen:
# Euklid:
TfidfDistances <- as.matrix(stats::dist(TfidfWeights))

return(list(TfidfDistances = TfidfDistances, TfidfWeights = TfidfWeights, TermFrequency = Tf, InverseDocFreq = Idf))
}