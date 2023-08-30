\name{matdistl2dnorm}
\alias{matdistl2dnorm}
\title{
    Matrix of \eqn{L^2} distances between \eqn{L^2}-normed probability densities
}
\description{
    Computes the matrix of the \eqn{L^2} distances between several multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) \eqn{L^2}-normed probability densities, estimated from samples, where a \eqn{L^2}-normed probability density is the original probability density function divided by its \eqn{L^2}-norm.
}
\usage{
matdistl2dnorm(x, method = "gaussiand", varwL = NULL)
}
\arguments{
  \item{x}{
       object of class "folder" containing the data. Its elements have only numeric variables (observations of the probability densities).
       If there are non numeric variables, there is an error.
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
    Positive symmetric matrix whose order is equal to the number of densities, consisting of the pairwise distances between the \eqn{L^2}-normed probability densities. 
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{distl2dnorm}}.
   
   \code{\link{matdistl2d}} for the distance matrix between probability densities.
   
   \code{\link{matdistl2dnormpar}} when the probability densities are Gaussian, given the parameters (means and variances).
}
\examples{
    data(roses)
    
    # Multivariate:
    X <- as.folder(roses[,c("Sha","Den","Sym","rose")], groups = "rose")
    summary(X)
    mean.X <- mean(X)
    var.X <- var.folder(X)
    
    # Parametrically estimated Gaussian densities:
    matdistl2dnorm(X)
    
    \dontrun{
    # Estimated densities using the Gaussian kernel method ()normal reference rule bandwidth):
    matdistl2dnorm(X, method = "kern")   

    # Estimated densities using the Gaussian kernel method (bandwidth provided):
    matdistl2dnorm(X, method = "kern", varwL = var.X)
    }

    # Univariate :
    X1 <- as.folder(roses[,c("Sha","rose")], groups = "rose")
    summary(X1)
    mean.X1 <- mean(X1)
    var.X1 <- var.folder(X1)
    
    # Parametrically estimated Gaussian densities:
    matdistl2dnorm(X1)
    
    # Estimated densities using the Gaussian kernel method (normal reference rule bandwidth):
    matdistl2dnorm(X1, method = "kern")
    
    # Estimated densities using the Gaussian kernel method (normal reference rule bandwidth):
    matdistl2dnorm(X1, method = "kern", varwL = var.X1)
}
