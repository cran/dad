distl2d <-
function(x1, x2, method="gaussiand", check=FALSE, varw1=NULL, varw2=NULL)  {
  # x1, x2 :       samples
  return(sqrt(l2d(x1, x1, method=method, check=check, varw1=varw1, varw2=varw1)
              + l2d(x2, x2, method=method, check=check, varw1=varw2, varw2=varw2)
              - 2*l2d(x1, x2, method=method, check=check, varw1=varw1, varw2=varw2)))
}
