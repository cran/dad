distl2dnorm <-
function(x1, x2, method="gaussiand", check=FALSE, varw1=NULL, varw2=NULL)  {
  # x1, x2 :       samples
  n1 <- sqrt(l2d(x1, x1, method=method, check=check, varw1=varw1, varw2=varw1))
  n2 <- sqrt(l2d(x2, x2, method=method, check=check, varw1=varw2, varw2=varw2))
  w12 <- l2d(x1, x2, method=method, check=check, varw1=varw1, varw2=varw2)
  return(sqrt(2 - 2*w12/(n1*n2)))
}
