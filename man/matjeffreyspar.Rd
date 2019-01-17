\name{matjeffreyspar}
\alias{matjeffreyspar}
\title{
    Matrix of Jeffreys measures (symmetrised Kullback-Leibler divergences) between Gaussian densities
}
\description{
    Computes the matrix of the Jeffreys measures between several multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) Gaussian densities, given their parameters (mean vectors and covariance matrices if the densities are multivariate, or means and variances if univariate), using \code{\link{jeffreyspar}}.
}
\usage{
matjeffreyspar(meanL, varL)
}
\arguments{
  \item{meanL}{
       list of the means (\eqn{p = 1}) or vector means (\eqn{p > 1}) of the Gaussian densities.
}
  \item{varL}{
       list of the variances (\eqn{p = 1}) or covariance matrices (\eqn{p > 1}) of the probability densities.
}
}
\value{
    Positive symmetric matrix whose order is equal to the number of densities, consisting of the pairwise Jeffreys measures between the Gaussian densities. 
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{jeffreyspar}}.
   
   \code{\link{matjeffreys}} for the matrix of Jeffreys divergences between probability densities which are estimated from the data.
}
\examples{
    data(roses)
    
    # Multivariate:
    X <- roses[,c("Sha","Den","Sym","rose")]
    summary(X)
    mean.X <- as.list(by(X[, 1:3], X$rose, colMeans))
    var.X <- as.list(by(X[, 1:3], X$rose, var))
    matjeffreyspar(mean.X, var.X)

    # Univariate :
    X1 <- roses[,c("Sha","rose")]
    summary(X1)
    mean.X1 <- by(X1$Sha, X1$rose, mean)
    var.X1 <- by(X1$Sha, X1$rose, var)
    matjeffreyspar(mean.X1, var.X1)
}
