\name{l2d.gs.u}
\alias{l2d.gs.u}
\title{
    \eqn{L^2} inner product of univariate Gaussian densities
}
\description{
    Computes the \eqn{L^2} inner product of univariate Gaussian densities, parametrically estimated from samples.
}
\usage{
l2d.gs.u(x1, x2, check=FALSE)
}
\arguments{
  \item{x1, x2}{
       numerical vectors. The observations of each sample.
}
\item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the variances of the samples are not zero, before computing the inner product. 
}
}
\details{
    Computes the means (\eqn{m1} and \eqn{m2}) and the variances (\eqn{v1} and \eqn{v2}) of the two samples, then uses
    \link{l2d.gp.u} to compute the inner product equal to: 
    
    \deqn{(2\pi)^{-1/2} (v1+v2)^{-1/2} \exp(-\frac{1}{2}\frac{(m1-m2)^2}{v1+v2})}{%
          (2 pi)^{-1/2} (v1+v2)^{-1/2} exp(-1/2 (m1-m2)^2/(v1+v2))}
}
\value{
    Returns the \eqn{L^2} inner product of the two Gaussian densities associated to the samples.
    Be careful! If \code{check=FALSE} and one variance is zero, the result returned must not be considered.
}
\references{
   Boumaza, R. (1998). Analyse en composantes principales de distributions gaussiennes multidimensionnelles. Revue de Statistique Appliquée, XLVI (2), 5-20.
}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}
\seealso{
   \link{l2d.gs} for multivariate densities; \link{l2d.gp.u} for Gaussian densities, given their parameters; \link{l2d.kga.u} and 
\link{l2d.kgw.u} for estimated densities using the Gaussian kernel method. 
}
\examples{
x1=rnorm(n=3,m=0,sd=1)
x2=rnorm(n=5,m=1,sd=2)
l2d.gs.u(x1,x2)
}
