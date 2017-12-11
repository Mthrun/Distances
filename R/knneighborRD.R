knneighborRD <- function(x, k, data, defined = rep(1, ncol(data))) {
  # author: Raphael Päbst
  warning('Not verified, may not work properly. Please check results')
dv <- dist2AllRD(x, data, defined)
nn <- sort(na.last=NA,dv, index.return = TRUE)[[2]]
nn <- nn[1 : k]

return(nn)
}