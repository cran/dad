distl2dnormpar <-
function(mean1, var1, mean2, var2, check=FALSE)  
{
  # mean1, var1 :   mean et variance (matrix) of the first data set.
  # mean2, var2 :   les (vecteur) moyenne et (matrice de) variance du deuxi?me jeu de donn?es.
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
  n1 <- sqrt(l2dpar(mean1, var1, mean1, var1))
  n2 <- sqrt(l2dpar(mean2, var2, mean2, var2))
  w12 <- l2dpar(mean1, var1, mean2, var2)
  return(sqrt(2 - 2*w12/(n1*n2)))
}

