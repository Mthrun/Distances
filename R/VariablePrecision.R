VariablePrecision= function(Variable){

# [MinAbsDiff,MinAbsNZDiff,MinExpo] = VariablePrecision(Variable)
# find he smallest (NonZero) Difference of any Value in Variable
#
# INPUT
# Variable(1:n,1:d)    numeric data or
#
# OUTPUT
# MinAbsDiff(1:d)        Absolute value of the smallest Difference of any Value in Variable
# MinAbsNZDiff(1:d)      Absolute value of the smallest NonZero Difference of any Value in Variable
# MinExpo(1:d)           The smalest Nachkommastelle in Variable NOT JET IMPLEMENTED
  
  
  n = dim(Variable)[1]
  d = dim(Variable)[2]
  
  if (is.null(d)) {
    Diffs = InnerVariableDifferences(Variable)
    MinAbsDiff  = min(abs(as.vector(Diffs)))
    MinAbsNZDiff = min(Diffs[Diffs > 0])
    MinExpo = NaN
  } else{
    # d>1
    MinAbsDiff = zeros(d, 1)
    MinAbsNZDiff = zeros(d, 1)
    MinExpo = zeros(d, 1)
    
    for (i in 1:d) {
      Diffs = InnerVariableDifferences(Variable[, i])
      MinAbsDiff[i]=min(abs(as.vector(Diffs)))
      MinAbsNZDiff[i] = min(Diffs[Diffs > 0])
      
      MinExpo[i] = NaN
    }  # for i
  }# if d = 1
  return(list(MinAbsDiff=MinAbsDiff, MinAbsNZDiff=MinAbsNZDiff, MinExpo=MinExpo))
}