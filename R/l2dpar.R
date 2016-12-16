l2dpar <-
function(mean1,var1,mean2,var2,check=FALSE)
{
  # L2-inner product between two gaussian distributions
  # when the parameters are given.
  
  p=length(mean1);
  d=mean1-mean2;
  vars=var1+var2;
  
  if (p == 1)
   {
      # Univariate distributions:
      if(check)
      {if(abs(var1)<.Machine$double.eps | abs(var2)<.Machine$double.eps)
        {stop("At least one variance is zero")
        }
      }
      return((1/sqrt(2*pi))*(1/sqrt(vars))*exp(-(1/2)*(d^2)/vars))
   } else
   {
      # Multivariate distributions:
      if(check)
      {if(abs(det(var1))<.Machine$double.eps | abs(det(var2))<.Machine$double.eps)
        {stop("One of the sample variances is degenerate")
        }
      }
      return(as.numeric((1/(2*pi)^(p/2))*(1/det(vars)^(1/2))*exp((-1/2)*t(d)%*%solve(vars)%*%d)))
   }
}

