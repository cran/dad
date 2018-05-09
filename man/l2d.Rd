\name{l2d}
\alias{l2d}
\alias{l2d.gs}
\alias{l2d.gs.u}
\alias{l2d.kgw}
\alias{l2d.kgw.u}
\alias{l2d.kga}
\alias{l2d.kga.u}
\title{
     \eqn{L^2} inner product of probability densities
}
\description{
    \eqn{L^2} inner product of two multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) probability densities, estimated from samples.
}
\usage{
l2d(x1, x2, method = "gaussiand", check = FALSE, varw1 = NULL, varw2 = NULL)
}
\arguments{
  \item{x1}{
       a matrix or data frame of \eqn{n_1}{n1} rows (observations) and \eqn{p} columns (variables) or a vector of length \eqn{n_1}{n1}.
}
  \item{x2}{
       matrix or data frame of \eqn{n_2}{n2} rows and \eqn{p} columns or vector of length \eqn{n_2}{n2}.
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
       \eqn{p} x \eqn{p} symmetric matrices: the smoothing bandwidths for the estimation of the probability densities. If they are omitted, the smoothing bandwidths are computed using the normal reference rule matrix bandwidth (see details).
}
}
\details{
    \itemize{
    \item If \code{method = "gaussiand"}, the mean vectors and the variance matrices (\eqn{v1} and \eqn{v2}) of the two samples are computed, and they are used to compute the inner product using the \code{\link{l2dpar}} function.
            
    \item If \code{method = "kern"}, the densities of both samples are estimated using the Gaussian kernel method.
    These estimations are then used to compute the inner product.
    if \code{varw1} and \code{varw2} arguments are omitted, the smoothing bandwidths are computed using the normal reference rule matrix bandwidth:
    \deqn{h_1 v_1^{1/2}}
where
    \deqn{h_1 = (4 / ( n_1 (p+2) ) )^{1 / (p+4)}}
for the first density.  Idem for the second density after making the necessary changes.
    }
}
\value{
    The \eqn{L^2} inner product of the two probability densities.
    
    Be careful! If \code{check = FALSE} and one smoothing bandwidth matrix is degenerate, the result returned can not be considered.
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.

   Wand, M., Jones, M. (1995). Kernel smoothing. Chapman and Hall/CRC, London.

   Yousfi, S., Boumaza R., Aissani, D., Adjabi, S. (2014). Optimal bandwith matrices in functional principal component analysis of density functions. Journal of Statistical Computational and Simulation, 85 (11), 2315-2330.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \link{l2dpar} for Gaussian densities, the parameters being given.
}
\examples{
require(MASS)
m1 <- c(0,0)
v1 <- matrix(c(1,0,0,1),ncol = 2) 
m2 <- c(0,1)
v2 <- matrix(c(4,1,1,9),ncol = 2)
x1 <- mvrnorm(n = 3,mu = m1,Sigma = v1)
x2 <- mvrnorm(n = 5, mu = m2, Sigma = v2)
l2d(x1, x2, method = "gaussiand")
l2d(x1, x2, method = "kern")
l2d(x1, x2, method = "kern", varw1 = v1, varw2 = v2)
}
