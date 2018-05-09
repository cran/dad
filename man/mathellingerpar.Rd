\name{mathellingerpar}
\alias{mathellingerpar}
\title{
    Matrix of Hellinger distances between Gaussian densities given their parameters
}
\description{
     Computes the matrix of the Hellinger distances between several multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) Gaussian densities, given their means and variances, using \code{\link{hellingerpar}}.
}
\usage{
mathellingerpar(meanL, varL)
}
\arguments{
  \item{meanL}{
       list of the means (\eqn{p = 1}) or vector means (\eqn{p > 1}) of the Gaussian densities.
}
  \item{varL}{
       list of the variances (\eqn{p = 1}) or covariance matrices (\eqn{p > 1}) of the Gaussian densities.
}
}
\value{
    Positive symmetric matrix whose order is equal to the number of densities, consisting of the pairwise distances between the Gaussian densities. 
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{hellingerpar}}.
   
   \code{\link{mathellinger}} for the distance matrix between probability densities which are estimated from the data.
   
   \code{\link{matdistl2d}}, \code{\link{matdistl2dpar}} for the matrix of \eqn{L^2}-distances.
}
\examples{
    data(roses)
    
    # Multivariate:
    X <- roses[,c("Sha","Den","Sym","rose")]
    summary(X)
    mean.X <- as.list(by(X[, 1:3], X$rose, colMeans))
    var.X <- as.list(by(X[, 1:3], X$rose, var))
    mathellingerpar(mean.X, var.X)

    # Univariate :
    X1 <- roses[,c("Sha","rose")]
    summary(X1)
    mean.X1 <- by(X1$Sha, X1$rose, mean)
    var.X1 <- by(X1$Sha, X1$rose, var)
    mathellingerpar(mean.X1, var.X1)
}
