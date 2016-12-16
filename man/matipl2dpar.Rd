\name{matipl2dpar}
\alias{matipl2dpar}
\alias{mat.ip.l2d.gp}
\alias{mat.ip.l2d.gp.u}
\title{
    Matrix of \eqn{L^2} inner products 
}
\description{
    Computes the matrix of \eqn{L^2} inner products between several \eqn{p}-dimensional Gaussiand probability densities, using \code{\link{l2dpar}}.
}
\usage{
matipl2dpar(meanL, varL)
}
\arguments{
  \item{meanL}{
       list of the means of the probability densities.
}
  \item{varL}{
       list of the variances (\eqn{p=1}) or covariance matrices (\eqn{p>1}) of the probability densities.
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
    
    # Gaussian densities, given parameters
    matipl2dpar(mean.X, var.X)

    # Univariate :
    X1 <- roses[,c("Sha","rose")]
    summary(X1)
    mean.X1 <- by(X1$Sha, X1$rose, mean)
    var.X1 <- by(X1$Sha, X1$rose, var)
    
    # Gaussian densities, given parameters
    matipl2dpar(mean.X1, var.X1)
}
