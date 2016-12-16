\name{matdistl2d}
\alias{matdistl2d}
\alias{mat.dist.l2d.gs}
\alias{mat.dist.l2d.gs.u}
\alias{mat.dist.l2d.kga}
\alias{mat.dist.l2d.kga.u}
\alias{mat.dist.l2d.kgw}
\alias{mat.dist.l2d.kgw.u}
\title{
    Matrix of \eqn{L^2} distances
}
\description{
    Computes the matrix of \eqn{L^2} distances between several \eqn{p}-dimensional probability densities (\eqn{p \geq 1}), using \code{\link{distl2d}}.
}
\usage{
matdistl2d(x, method = "gaussiand", varwL = NULL)
}
\arguments{
  \item{x}{
       data frame with \eqn{p+1} columns containing the data. The last column is a factor (the groups). The first \eqn{p} columns contain numeric data (observations of the probability densities).
}
  \item{method}{
       string.  It can be:
       \itemize{
       \item \code{"gaussiand"} if the densities are considered to be Gaussian.
       \item \code{"kern"} if they are estimated using the Gaussian kernel method.
       }
}
  \item{varwL}{
       list of matrices. The smoothing bandwidths for the estimation of each probability density. If they are omitted, the smoothing bandwidths are computed using the normal reference rule matrix bandwidth (see details).
}
}
\value{
    Positive symmetric matrix whose order is equal to the number of densities, consisting of the pairwise distances between the probability densities. 
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{distl2d}}.
   \code{\link{matdistl2dpar}} when the probability densities are Gaussian, given the parameters (means and variances).
}
\examples{
    data(roses)
    
    # Multivariate:
    X <- roses[,c("Sha","Den","Sym","rose")]
    summary(X)
    mean.X <- as.list(by(X[, 1:3], X$rose, colMeans))
    var.X <- as.list(by(X[, 1:3], X$rose, var))
    
    # Parametrically estimated Gaussian densities:
    matdistl2d(X)
    
    \dontrun{
    # Estimated densities using the Gaussian kernel method ()normal reference rule bandwidth):
    matdistl2d(X, method = "kern")   

    # Estimated densities using the Gaussian kernel method (bandwidth provided):
    matdistl2d(X, method = "kern", varwL = var.X)
    }

    # Univariate :
    X1 <- roses[,c("Sha","rose")]
    summary(X1)
    mean.X1 <- by(X1$Sha, X1$rose, mean)
    var.X1 <- by(X1$Sha, X1$rose, var)
    
    # Parametrically estimated Gaussian densities:
    matdistl2d(X1)
    
    # Estimated densities using the Gaussian kernel method (normal reference rule bandwidth):
    matdistl2d(X1, method = "kern")
    
    # Estimated densities using the Gaussian kernel method (normal reference rule bandwidth):
    matdistl2d(X1, method = "kern", varwL = var.X1)
}
