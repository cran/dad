distl2dpar <-
function(mean1, var1, mean2, var2, check=FALSE)  
{
  # mean1, var1 :   mean and variance (matrix) of the first dataset.
  # mean2, var2 :   the mean (vector) and variance (matrix) of the second dataset.
   if(check)
    {if (length(mean1) == 1)
      {if(abs(var1)<.Machine$double.eps | abs(var2)<.Machine$double.eps)
        {stop("At least one variance is zero")
        }
      } else
      {if(abs(det(var1))<.Machine$double.eps | abs(det(var2))<.Machine$double.eps )
        {stop("One of the sample variances is degenerate")
        }
      }
    }  
  return(sqrt(l2dpar(mean1, var1, mean1, var1) + l2dpar(mean2, var2, mean2, var2) - 2*l2dpar(mean1, var1, mean2, var2)))
}

