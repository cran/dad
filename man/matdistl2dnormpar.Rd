\name{matdistl2dnormpar}
\alias{matdistl2dnormpar}
\title{
    Matrix of \eqn{L^2} distances between \eqn{L^2}-normed Gaussian densities given their parameters
}
\description{
     Computes the matrix of the \eqn{L^2} distances between several multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) \eqn{L^2}-normed Gaussian densities, given their parameters (mean vectors and covariance matrices if the densities are multivariate, or means and variances if univariate), where a \eqn{L^2}-normed Gaussian density is the original probability density function divided by its \eqn{L^2}-norm.
}
\usage{
matdistl2dnormpar(meanL, varL)
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
    Positive symmetric matrix whose order is equal to the number of densities, consisting of the pairwise distances between the \eqn{L^2}-normed probability densities. 
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{distl2dnormpar}}.
   
   \code{\link{matdistl2dpar}} for the distance matrix between Gaussian densities, given their parameters.
   
   \code{\link{matdistl2dnorm}} for the distance matrix between normed probability densities which are estimated from the data.
}
\examples{
    data(roses)
    
    # Multivariate:
    X <- roses[,c("Sha","Den","Sym","rose")]
    summary(X)
    mean.X <- as.list(by(X[, 1:3], X$rose, colMeans))
    var.X <- as.list(by(X[, 1:3], X$rose, var))
    
    # Gaussian densities, given parameters
    matdistl2dnormpar(mean.X, var.X)

    # Univariate :
    X1 <- roses[,c("Sha","rose")]
    summary(X1)
    mean.X1 <- by(X1$Sha, X1$rose, mean)
    var.X1 <- by(X1$Sha, X1$rose, var)
    
    # Gaussian densities, given parameters
    matdistl2dnormpar(mean.X1, var.X1)
}
