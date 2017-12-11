 KullbLeiblKLD=function(P,Q,sym=1){
# [KLD,p,q,x] = KullbLeiblKLD(P,Q,sym);
# Kullback-Leibler divergence KLD of discrete distributions
# 
# INPUT
# P, Q        two distributions having the same dirccrete range unique([P,Q])
#             NaN in the distributions are ignored.
#
# OPTIONAL 
# sym         default ==1: if sym ==1 then the symmetical KLD  is returned
#
# OUPUT
# KLD         Kullback-Leibler divergence
# p,q         frequency counts of distributions
# x           the unique discrete x values of P and Q: x = unique([P;Q])

# not defined for zero probabilities, replace with very small values
# reimplemented from matlab by MT 01/2016
EPS = 1/10000
P=P[!is.na(P)]
Q=Q[!is.na(Q)]
x = sort(na.last=T,unique(c(P,Q)))
AnzUniqX = length(x)  # anzal verschiedener x werte
if(AnzUniqX<2){ # triviale verteilung
    KLD   = 0
    KLDpq = 0
    KLDqp = 0
    p=1
    q = 1
}else{ # mindestens 2 Elemente in der Verteilung
  FreqP=vector()
  for(i in 1:length(x))
    FreqP[i]=length(which(P==x[i]))
    #MT funktioniert nicht!
    #FreqP = hist(P,x,plot=F)$counts # haufigkeiten in P
    #FreqQ = hist(Q,x,plot=F)$counts # haufigkeiten in Q
  FreqQ=vector()
  for(i in 1:length(x))
    FreqQ[i]=length(which(Q==x[i]))

    p  = FreqP/sum(FreqP) # wahrschinlichkeiten in P
    q  = FreqQ/sum(FreqQ) # wahrschinlichkeiten in Q


    LogP    = vector("numeric",AnzUniqX)*NaN # default ist nan damit die summen hinterher stimmen
    LogQ    = vector("numeric",AnzUniqX)*NaN # default ist nan damit die summen hinterher stimmen
    LogPzuQ = vector("numeric",AnzUniqX)*NaN # default ist nan damit die summen hinterher stimmen
    LogQzuP = vector("numeric",AnzUniqX)*NaN # default ist nan damit die summen hinterher stimmen

    Ind = which(p>EPS,arr.ind=T)
    LogP[Ind] = log(p[Ind])
    Ind = which(q>EPS,arr.ind=T)
    LogQ[Ind] = log(q[Ind])
    # Brueche ausrechnen
    Ind = which((q>EPS)&(p>EPS),arr.ind=T)
    LogPzuQ[Ind] = log(p[Ind]/q[Ind])
    Ind = which((q>EPS)&(p>EPS),arr.ind=T)
    LogQzuP[Ind] = log(q[Ind]/p[Ind])
# 
#     # KLD(p,q) ausrechnen
     KLDpq = LogP* LogPzuQ
    ZeroInd = which(p<=EPS,arr.ind=T)
    KLDpq[ZeroInd] =0; 
#     # KLD(q,p) ausrechnen
     KLDqp = LogQ* LogQzuP
    ZeroInd = which(q<=EPS,arr.ind=T)
    KLDqp[ZeroInd] =0;
# 
    KLDpq[!is.finite(KLDpq)] = NaN
    KLDqp[!is.finite(KLDqp)] = NaN

    KLDpqS = sum(KLDpq[is.finite(KLDpq)],na.rm=T) /AnzUniqX
    KLDqpS = sum(KLDqp[is.finite(KLDqp)],na.rm=T)/ AnzUniqX

    # welche KLD soll zurueckgegeben werden
    if(sym ==1){
        KLD = KLDqpS+KLDpqS  # symmetrische KLD 
    }else{
        KLD =  KLDpqS
    }
} # if length(x)<2 # triviale verteilung
return(list(KLD=KLD,p=p,q=q,x=x))
 }



