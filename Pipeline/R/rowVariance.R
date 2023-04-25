rowVariance <- function (X,na.rm = TRUE) 
{
  sqr = function(X) X * X
  n = rowSums(!is.na(X))
  n[n <= 1] = NA
  return(rowSums(sqr(X - rowMeans(X,na.rm = na.rm)), na.rm = na.rm)/(n - 1))
}