jeffreyspar <- function(mean1,var1,mean2,var2,check=FALSE)
{
  # Jeffreys distance (Kullback-Leibler divergence) between two gaussian distributions
  # when the parameters are given.
  #
  # x1, x2: the data.
  
  p <- length(mean1)
  
  if (p == 1)
  {
    if (check)
    {
      if(var1 < .Machine$double.eps | var2 < .Machine$double.eps)
        stop("At least one variance is zero")
    }
    
    d <- mean1 - mean2
    ivar1 <- 1/var1
    ivar2 <- 1/var2
    
    return(as.numeric((d^2*(ivar1+ivar2) - (var1-var2)*(ivar1-ivar2))/2))
  } else {
    if (check)
    {
      if(abs(det(var1)) < .Machine$double.eps | abs(det(var2)) < .Machine$double.eps)
        stop("One of the sample variances is degenerate")
    }
    
  d <- mean1 - mean2
  ivar1 <- solve(var1)
  ivar2 <- solve(var2)
    
  return(as.numeric((t(d)%*%(ivar1+ivar2)%*%d - sum(diag( (var1-var2)%*%(ivar1-ivar2) )))/2))
  }
}
