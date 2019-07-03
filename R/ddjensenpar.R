ddjensenpar <- function(p1, p2) {
  # Jensen-Shannon divergence
  
  k <- length(dim(p1))
  if (length(dim(p2)) != k)
    stop("p1 and p2 must have the same number of dimensions.")
  
  if (k == 1) {
    # If univariate
    if (length(p1) != length(p2))
      stop("p1 and p2 must be vectors with the same length.")
    dinv <- 2/(p1 + p2)
    dd1 <- p1*log(p1*dinv)
    dd2 <- p2*log(p2*dinv)
    return(sum(dd1 + dd2, na.rm = TRUE))
  } else {
    # Multivariate
    dd1 <- as.numeric(p1)*log(as.numeric(p1)*2/(p1 + p2))
    dd2 <- as.numeric(p2)*log(as.numeric(p2)*2/(p1 + p2))
    return(sum(dd1 + dd2, na.rm = TRUE))
  }
}
