jeffreys <- function(x1, x2, check = FALSE)
{
  # Jeffreys measure (Symmetrised Kullback-Leibler divergence) between two Gaussian distributions
  #
  # x1, x2: the data.
  
  p <- NCOL(x1)
  if (NCOL(x2) != p)
    stop("x1 and x2 must be two vectors, or have the same number of columns.")
  
  if (p == 1)
  {
    if (is.data.frame(x1)) x1 <- x1[, 1]
    if (is.data.frame(x2)) x2 <- x2[, 1]
    if (is.matrix(x1)) x1 <- drop(x1)
    if (is.matrix(x2)) x2 <- drop(x2)
    return(jeffreyspar(mean(x1),var(x1),mean(x2),var(x2),check=check))
  } else {
    return(jeffreyspar(colMeans(x1),var(x1),colMeans(x2),var(x2),check=check))
  }
}
