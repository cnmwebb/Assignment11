a=diag(c(1,2,3))
tukey_multiple <- function(x) {
  outliers <- array(TRUE,dim=dim(x))
  for (j in 1:ncol(x))
  {
    outliers[,j] <- outliers[,j] && outliers(x[,j])
  }
  outliers.vec <- vector(length=nrow(x))
  for (i in 1:nrow(x))
  { outliers.vec[i] <- all(outliers[i,])
  return(outliers.vec)}}

debug(tukey_multiple)
tukey_multiple(a)

