\name{mat.ip.l2d}
\alias{mat.ip.l2d}
\alias{mat.ip.l2d.gp}
\alias{mat.ip.l2d.gp.u}
\alias{mat.ip.l2d.gs}
\alias{mat.ip.l2d.gs.u}
\alias{mat.ip.l2d.kgw}
\alias{mat.ip.l2d.kgw.u}
\alias{mat.ip.l2d.kga}
\alias{mat.ip.l2d.kga.u}
\title{
    Matrix of \eqn{L^2} inner products 
}
\description{
    Computes the matrix of \eqn{L^2} inner products between several \eqn{p}-dimensional probability densities.
}
\usage{
mat.ip.l2d.gp(meanL, varL)
mat.ip.l2d.gp.u(meanL, varL)
mat.ip.l2d.gs(x)
mat.ip.l2d.gs.u(x)
mat.ip.l2d.kga(x)
mat.ip.l2d.kga.u(x)
mat.ip.l2d.kgw(x, varwL)
mat.ip.l2d.kgw.u(x, varwL)
}
\arguments{
  \item{meanL}{
       list of the means of the probability densities.
}
  \item{varL}{
       list of the variances (\eqn{p=1}) or covariance matrices (\eqn{p>1}) of the probability densities.
}
  \item{x}{
       data frame with \eqn{p+1} columns containing the data. The last column is a factor (the groups). The first \eqn{p} columns contain numeric data (observations of the probability densities).
}
  \item{varwL}{
       list of matrices. The smoothing bandwidths for the estimation of each probability density.
}
}
\details{
The functions \code{mat.ip.l2d...} whose names end by '\code{.u}' apply to unidimensional densities and the others apply to \eqn{p}-dimensional densities with \eqn{p>=2}.
They use the corresponding inner product function \link{l2d} to calculate the pairwise inner products.
}

\value{
    Positive symmetric matrix whose order is equal to the number of densities, consisting of the pairwise inner products between the probability densities. 
}

\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}
\examples{
    data(roses)
    
    # Multivariate:
    X = roses[,c("Sha","Den","Sym","Lot")]
    summary(X)
    mean.X = as.list(by(X[, 1:3], X$Lot, colMeans))
    var.X = as.list(by(X[, 1:3], X$Lot, var))
    
    # Gaussian densities, given parameters
    mat.ip.l2d.gp(mean.X, var.X)
    
    # Parametrically estimated Gaussian densities:
    mat.ip.l2d.gs(X)
    
    # Estimated densities using the Gaussian kernel method (normal reference rule bandwidth):
    mat.ip.l2d.kga(X)
    
    # Estimated densities using the Gaussian kernel method (normal reference rule bandwidth):
    mat.ip.l2d.kgw(X, var.X)
    
    # Univariate :
    X1 = roses[,c("Sha","Lot")]
    summary(X1)
    mean.X1 = by(X1$Sha, X1$Lot, mean)
    var.X1 = by(X1$Sha, X1$Lot, var)
    
    # Gaussian densities, given parameters
    mat.ip.l2d.gp.u(mean.X1, var.X1)
    
    # Parametrically estimated Gaussian densities:
    mat.ip.l2d.gs.u(X1)
    
    # Estimated densities using the Gaussian kernel method (normal reference rule bandwidth):
    mat.ip.l2d.kga.u(X1)
    
    # Estimated densities using the Gaussian kernel method (normal reference rule bandwidth):
    mat.ip.l2d.kgw.u(X1, var.X1)
}
