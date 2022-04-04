# The original code. We are asked to fixa bug !
tukey_multiple <- function(x) {
  outliers <- array(TRUE,dim=dim(x))
  for (j in 1:ncol(x))
  {
    outliers[,j] <- outliers[,j] && tukey.outlier(x[,j])
  }
  outlier.vec <- vector(length=nrow(x))
  for (i in 1:nrow(x))
  { outlier.vec[i] <- all(outliers[i,]) } return(outlier.vec) }


#bug fixed. The bug: make sure curly brackets that close a function 
# and the the ones defining an if statement are not on the same line
#because it confuses R which brackets belong to which function or
#if statement. It is a standard practise in R and other languages to
#put {} on sperate lines and not just for stylistic choices. This 
#could affect R's ability to intrepret the syntax. 
fixed_tukey_multiple <- function(x) {
  outliers <- array(TRUE,dim=dim(x))
  for (j in 1:ncol(x))
  {
    outliers[,j] <- outliers[,j] && tukey.outlier(x[,j])
  }
  outlier.vec <- vector(length=nrow(x))
  for (i in 1:nrow(x))
  { outlier.vec[i] <- all(outliers[i,]) }
  return(outlier.vec) 
  }

