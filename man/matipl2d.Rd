\name{matipl2d}
\alias{matipl2d}
\alias{mat.ip.l2d.gs}
\alias{mat.ip.l2d.gs.u}
\alias{mat.ip.l2d.kga}
\alias{mat.ip.l2d.kga.u}
\alias{mat.ip.l2d.kgw}
\alias{mat.ip.l2d.kgw.u}
\title{
    Matrix of \eqn{L^2} inner products 
}
\description{
    Computes the matrix of \eqn{L^2} inner products between several \eqn{p}-dimensional probability densities, using \code{\link{l2d}}.
}
\usage{
matipl2d(x, method = "gaussiand", varwL = NULL)
}
\arguments{
  \item{x}{
       data frame with \eqn{p+1} (\eqn{p \geq 1}) columns containing the data. The last column is a factor (the groups). The first \eqn{p} columns contain numeric data (observations of the probability densities).
}
  \item{method}{
       string.  It can be:
       \itemize{
       \item \code{"gaussiand"} if the densities are considered to be Gaussian.
       \item \code{"kern"} if they are estimated using the Gaussian kernel method.
       }
}
  \item{varwL}{
       list of matrices. The smoothing bandwidths for the estimation of each probability density. If they are omitted, the smoothing bandwidths are computed using the normal reference rule matrix bandwidth (see details of the \code{\link{l2d}} function).
}
}

\value{
    Positive symmetric matrix whose order is equal to the number of densities, consisting of the pairwise inner products between the probability densities. 
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\examples{
    data(roses)
    
    # Multivariate:
    X <- roses[,c("Sha","Den","Sym","rose")]
    summary(X)
    mean.X <- as.list(by(X[, 1:3], X$rose, colMeans))
    var.X <- as.list(by(X[, 1:3], X$rose, var))
    
    # Parametrically estimated Gaussian densities:
    matipl2d(X)
    
    # Estimated densities using the Gaussian kernel method (normal reference rule bandwidth):
    matipl2d(X, method  = "kern")
    
    # Estimated densities using the Gaussian kernel method (bandwidth provided):
    matipl2d(X, method  = "kern", varwL = var.X)
    
    # Univariate :
    X1 <- roses[,c("Sha","rose")]
    summary(X1)
    mean.X1 <- by(X1$Sha, X1$rose, mean)
    var.X1 <- by(X1$Sha, X1$rose, var)
    
    # Parametrically estimated Gaussian densities:
    matipl2d(X1)
    
    # Estimated densities using the Gaussian kernel method (normal reference rule bandwidth):
    matipl2d(X1, method = "kern")
    
    # Estimated densities using the Gaussian kernel method (bandwidth provided):
    matipl2d(X1, method = "kern", varwL = var.X1)
}
