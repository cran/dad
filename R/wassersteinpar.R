wassersteinpar <-
function(mean1,var1,mean2,var2,check=FALSE)
{
  # Wasserstein distance between two gaussian distributions
  # when the parameters are given.
  
  p <- length(mean1)
  d <- mean1-mean2
  vars <- var1+var2
  
  if (p == 1)
   {
      # Univariate distributions:
      if(check)
      {if(abs(var1) < .Machine$double.eps | abs(var2) < .Machine$double.eps)
        {stop("At least one variance is zero")
        }
      }
      return(sqrt( d^2 + var1 + var2 - 2*sqrt(var1*var2) ))
  } else
   {
      # Multivariate distributions:
      if(check)
        {
        if(abs(det(var1)) < .Machine$double.eps | abs(det(var2)) < .Machine$double.eps)
          {
          stop("One of the sample variances is degenerate")
          }
        }
     sqrtvar2 <- sqrtmatrix(var2)
     sqrtvars <- sqrtmatrix(sqrtvar2%*%var1%*%sqrtvar2)
     tracevar <- sum(diag(vars - 2*sqrtvars))
     
     return( sqrt( sum(d^2) + tracevar ) )
   }
}

