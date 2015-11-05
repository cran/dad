\name{mat.dist.l2d}
\alias{mat.dist.l2d}
\alias{mat.dist.l2d.gp}
\alias{mat.dist.l2d.gp.u}
\alias{mat.dist.l2d.gs}
\alias{mat.dist.l2d.gs.u}
\alias{mat.dist.l2d.kgw}
\alias{mat.dist.l2d.kgw.u}
\alias{mat.dist.l2d.kga}
\alias{mat.dist.l2d.kga.u}
\title{
    Matrix of \eqn{L^2} distances
}
\description{
    Computes the matrix of \eqn{L^2} distances between several \eqn{p}-dimensional probability densities.
}
\usage{
mat.dist.l2d.gp(meanL, varL)
mat.dist.l2d.gp.u(meanL, varL)
mat.dist.l2d.gs(x)
mat.dist.l2d.gs.u(x)
mat.dist.l2d.kgw(x, varwL)
mat.dist.l2d.kgw.u(x, varwL)
mat.dist.l2d.kga(x)
mat.dist.l2d.kga.u(x)
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
The functions compute the distance between \eqn{f_1} and \eqn{f_2} from the formula
 \deqn{||f_1 - f_2||^2 = ||f_1||^2 + ||f_2||^2 - 2 <f_1, f_2>.}
 The functions \code{mat.dist.l2d...} whose names end by '\code{.u}' apply to unidimensional densities and the others apply to \eqn{p}-dimensional densities with \eqn{p>=2}.

    For some information about the method used to compute the \eqn{L^2} inner product or about the arguments, see the corresponding function, that is for \code{dist.l2d.gp}, see \code{\link{l2d.gp}} and so on: 

    \code{dist.l2d.gp.u}, see \code{\link{l2d.gp.u}} 

    \code{dist.l2d.gs}, see \code{\link{l2d.gs}}  
                                                 
    \code{dist.l2d.gs.u}, see \code{\link{l2d.gs.u}}  

    \code{dist.l2d.kga}, see \code{\link{l2d.kga}} 
                                             
    \code{dist.l2d.kga.u}, see \code{\link{l2d.kga.u}} .

    \code{dist.l2d.kgw}, see \code{\link{l2d.kgw}}  

    \code{dist.l2d.kgw.u}, see \code{\link{l2d.kgw.u}}  
}
\value{
    Positive symmetric matrix whose order is equal to the number of densities, consisting of the pairwise inner products between the probability densities. 
}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}
\seealso{
   \code{\link{dist.l2d}}.
}
\examples{
    data(roses)
    
    # Multivariate:
    X = roses[,c("Sha","Den","Sym","Lot")]
    summary(X)
    mean.X = as.list(by(X[, 1:3], X$Lot, colMeans))
    var.X = as.list(by(X[, 1:3], X$Lot, var))
    
    # Gaussian densities, given parameters
    mat.dist.l2d.gp(mean.X, var.X)
    
    # Parametrically estimated Gaussian densities:
    mat.dist.l2d.gs(X)
    
    \dontrun{
    # Estimated densities using the Gaussian kernel method ()normal reference rule bandwidth):
    mat.dist.l2d.kga(X)   

    # Estimated densities using the Gaussian kernel method (normal reference rule bandwidth):
    mat.dist.l2d.kgw(X, var.X)
    }

    # Univariate :
    X1 = roses[,c("Sha","Lot")]
    summary(X1)
    mean.X1 = by(X1$Sha, X1$Lot, mean)
    var.X1 = by(X1$Sha, X1$Lot, var)
    
    # Gaussian densities, given parameters
    mat.dist.l2d.gp.u(mean.X1, var.X1)
    
    # Parametrically estimated Gaussian densities:
    mat.dist.l2d.gs.u(X1)
    
    # Estimated densities using the Gaussian kernel method (normal reference rule bandwidth):
    mat.dist.l2d.kga.u(X1)
    
    # Estimated densities using the Gaussian kernel method (normal reference rule bandwidth):
    mat.dist.l2d.kgw.u(X1, var.X1)
}
