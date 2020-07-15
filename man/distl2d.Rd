\name{distl2d}
\alias{distl2d}
%\alias{dist.l2d.gs}
%\alias{dist.l2d.gs.u}
%\alias{dist.l2d.kgw}
%\alias{dist.l2d.kgw.u}
%\alias{dist.l2d.kga}
%\alias{dist.l2d.kga.u}
\title{
     \eqn{L^2} distance between probability densities
}
\description{
    \eqn{L^2} distance between two multivariate (\eqn{p > 1}) or univariate (dimension: \eqn{p = 1}) probability densities, estimated from samples.
}
\usage{
distl2d(x1, x2, method = "gaussiand", check = FALSE, varw1 = NULL, varw2 = NULL)
}
\arguments{
  \item{x1, x2}{
       the samples from the probability densities (see \code{\link{l2d}}.
}
  \item{method}{
       string.  It can be:
       \itemize{
       \item \code{"gaussiand"} if the densities are considered to be Gaussian.
       \item \code{"kern"} if they are estimated using the Gaussian kernel method.
       }
}
  \item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the covariance matrices (if \code{method = "gaussiand"}) or smoothing bandwidth matrices (if \code{method = "kern"}) are not degenerate, before computing the inner product. 
       
       Notice that if \eqn{p = 1}, it checks if the variances or smoothing parameters are not zero.
}
  \item{varw1, varw2}{
       the bandwidths when the densities are estimated by the kernel method (see \code{\link{l2d}}.
}
}
\details{
    The function \code{distl2d} computes the distance between \eqn{f_1} and \eqn{f_2} from the formula
 \deqn{||f_1 - f_2||^2 = <f_1, f_1> + <f_2, f_2> - 2 <f_1, f_2>}
 
    For some information about the method used to compute the \eqn{L^2} inner product or about the arguments, see \code{\link{l2d}}.
}
    
\value{
    The \eqn{L^2} distance between the two densities.
    
    Be careful! If \code{check = FALSE} and one smoothing bandwidth matrix is degenerate, the result returned can not be considered.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{matdistl2d}} in order to compute pairwise distances between several densities.
}
\examples{
require(MASS)
m1 <- c(0,0)
v1 <- matrix(c(1,0,0,1),ncol = 2) 
m2 <- c(0,1)
v2 <- matrix(c(4,1,1,9),ncol = 2)
x1 <- mvrnorm(n = 3,mu = m1,Sigma = v1)
x2 <- mvrnorm(n = 5, mu = m2, Sigma = v2)
distl2d(x1, x2, method = "gaussiand")
distl2d(x1, x2, method = "kern")
distl2d(x1, x2, method = "kern", varw1 = v1, varw2 = v2)
}
