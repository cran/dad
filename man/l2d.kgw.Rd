\name{l2d.kgw}
\alias{l2d.kgw}
\title{
     \eqn{L^2} inner product of estimated probability densities
}
\description{
    Computes the \eqn{L^2} inner product of multivariate probability densities, estimated from samples and given matrix bandwidths, using the Gaussian kernel method.
}
\usage{
l2d.kgw(x1, varw1, x2, varw2, check = FALSE)
}
\arguments{
  \item{x1}{
       a matrix or data frame of \eqn{n_1}{n1} rows (observations) and \eqn{p} columns (variables).
}
  \item{varw1}{
       a \eqn{p \times p}-symmetric matrix: the smoothing bandwidth for the estimation of the first probability density.
}
  \item{x2}{
       matrix or data frame of \eqn{n_2}{n2} rows and \eqn{p} columns.
}
  \item{varw2}{
       a \eqn{p \times p}-symmetric matrix: the smoothing bandwidth for the estimation of the second probability density.
}
  \item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the smoothing bandwidth matrices are not degenerate, before computing the inner product. 
}
}
\details{
    The densities of both samples are estimated using the Gaussian kernel method.
    These estimations are then used to compute an estimation of the inner product.
}
\value{
    Returns the \eqn{L^2} inner product of the two probability densities.
    Be careful! If \code{check=FALSE} and one smoothing bandwidth matrix is degenerate, the result returned can not be considered.
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.

   Wand, M., Jones, M. (1995). Kernel smoothing. Chapman and Hall/CRC, London.

   Yousfi, S., Boumaza R., Aissani, D., Adjabi, S. (2014). Optimal bandwith matrices in functional principal component analysis of density functions. Journal of Statistical Computational and Simulation, 85 (11), 2315-2330.
}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}
\seealso{
   \link{l2d.kgw.u} for univariate densities; \link{l2d.kga} for estimated densities using the Gaussian kernel method, with the normal reference rule matrix bandwidth; \link{l2d.gp} and \link{l2d.gs} for Gaussian densities.
}
\examples{
require(MASS)
m1=c(0,0)
v1=matrix(c(1,0,0,1),ncol=2) 
m2=c(0,1)
v2=matrix(c(4,1,1,9),ncol=2)
x1=mvrnorm(n=3,mu=m1,Sigma=v1)
x2=mvrnorm(n=5,mu=m2,Sigma=v2)
l2d.kgw(x1, v1, x2, v2)
}
