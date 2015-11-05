\name{l2d.kgw.u}
\alias{l2d.kgw.u}
\title{
     \eqn{L^2} inner product of estimated probability densities
}
\description{
    Computes the \eqn{L^2} inner product of univariate probability densities, estimated from samples and given bandwidths, using the Gaussian kernel method.
}
\usage{
l2d.kgw.u(x1, varw1, x2, varw2, check=FALSE)
}
\arguments{
  \item{x1}{
       vector of the observations of the first sample.
}
  \item{varw1}{
       smoothing bandwidth for the estimation of the first probability density.
}
  \item{x2}{
       vector of the observations of the second sample.
}
  \item{varw2}{
       smoothing bandwidth for the estimation of the second probability density.
}
\item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the smoothing bandwidths are not zero, before computing the inner product. 
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

   Yousfi, S., Boumaza R., Aissani, D., Adjabi, S. (2014). Optimal bandwith matrices in functional principal component analysis of density functions. Journal of Statistical Computational and Simulation, 85 (11), 2315-2330.
}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}
\seealso{
   \link{l2d.kgw} for multivariate densities; \link{l2d.kga.u} for estimated densities using the Gaussian kernel method, with the normal reference rule bandwidth; \link{l2d.gp.u} and \link{l2d.gs.u} for Gaussian densities.
}
\examples{
m1=0; v1=1
m2=1; v2=4
x1=rnorm(n=3,m=m1,sd=sqrt(v1))
x2=rnorm(n=5,m=m2,sd=sqrt(v2))
l2d.kgw.u(x1, v1, x2, v2)
}
