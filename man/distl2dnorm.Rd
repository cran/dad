\name{distl2dnorm}
\alias{distl2dnorm}
\title{
     \eqn{L^2} distance between \eqn{L^2}-normed probability densities
}
\description{
    \eqn{L^2} distance between two multivariate (\eqn{p > 1}) or univariate (dimension: \eqn{p = 1}) \eqn{L^2}-normed probability densities, estimated from samples, where a \eqn{L^2}-normed probability density is the original probability density function divided by its \eqn{L^2}-norm.
}
\usage{
distl2dnorm(x1, x2, method = "gaussiand", check = FALSE, varw1 = NULL, varw2 = NULL)
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
    Given  densities \eqn{f_1} and \eqn{f_2}, the function \code{distl2dnormpar} computes the distance between the \eqn{L^2}-normed densities \eqn{f_1 / ||f_1||} and \eqn{f_2 / ||f_2||}:
    
 \deqn{2 - 2 <f_1, f_2> / (||f_1|| ||f_2||)}
 
    For some information about the method used to compute the \eqn{L^2} inner product or about the arguments, see \code{\link{l2d}}.
}
    
\value{
    The \eqn{L^2} distance between the two \eqn{L^2}-normed densities.
    
    Be careful! If \code{check = FALSE} and one smoothing bandwidth matrix is degenerate, the result returned can not be considered.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{distl2d}} for the distance between two probability densities.
   
   \code{\link{matdistl2dnorm}} in order to compute pairwise distances between several \eqn{L^2}-normed densities.
}
\examples{
require(MASS)
m1 <- c(0,0)
v1 <- matrix(c(1,0,0,1),ncol = 2) 
m2 <- c(0,1)
v2 <- matrix(c(4,1,1,9),ncol = 2)
x1 <- mvrnorm(n = 3,mu = m1,Sigma = v1)
x2 <- mvrnorm(n = 5, mu = m2, Sigma = v2)
distl2dnorm(x1, x2, method = "gaussiand")
distl2dnorm(x1, x2, method = "kern")
distl2dnorm(x1, x2, method = "kern", varw1 = v1, varw2 = v2)
}
