hellingerpar <-
function(mean1,var1,mean2,var2,check=FALSE)
{
  # L2-inner product between two gaussian distributions
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
    affin <- sqrt(2)* ((var1*var2)^(1/4) / sqrt(vars)) * exp((-1/4)*d^2/vars)
    return(sqrt(2 - 2*affin))
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
     affin <- 2^(p/2)* (det(var1%*%var2)^(1/4) / det(vars)^(1/2)) * 
       exp((-1/4)*t(d)%*%solve(vars)%*%d)
     return(sqrt(as.numeric(2 - 2*affin)))
   }
}

