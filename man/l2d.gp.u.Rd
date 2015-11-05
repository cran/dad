\name{l2d.gp.u}
\alias{l2d.gp.u}
\title{
    \eqn{L^2} inner product of univariate Gaussian densities
}
\description{
    Computes the \eqn{L^2} inner product of univariate Gaussian densities, given their parameters (means and variances).
}
\usage{
l2d.gp.u(mean1, var1, mean2, var2, check=FALSE)
}
\arguments{
  \item{mean1, var1}{
       parameters (mean and variance) of the first Gaussian density.
}
  \item{mean2, var2}{
       parameters of the second Gaussian density.
}
  \item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the variances are not zero, before computing the inner product. 
}
}
\details{
    Computes the inner product of the two univariate Gaussian densities, equal to: 
    
    \deqn{(2\pi)^{-1/2} (var1+var2)^{-1/2} \exp(-\frac{1}{2}\frac{(mean1-mean2)^2}{var1+var2})}{%
          (2 pi)^{-1/2} (var1+var2)^{-1/2} exp(-1/2 (mean1-mean2)^2/(var1+var2))}
}                                                                                                      
\value{
    Returns the \eqn{L^2} inner product of the two Gaussian densities.
    Be careful! If \code{check=FALSE} and one variance is zero, the result returned must not be considered.
}
\references{
   Boumaza, R. (1998). Analyse en composantes principales de distributions gaussiennes multidimensionnelles. Revue de Statistique Appliquée, XLVI (2), 5-20.
}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}
\seealso{
      \link{l2d.gp} for multivariate densities; \link{l2d.gs.u} for parametrically estimated Gaussian densities, given samples; 
      \link{l2d.kga.u} and \link{l2d.kgw.u} for estimated densities using the Gaussian kernel method.
}
\examples{
l2d.gp.u(0,1,1,4)
#
m1=0; v1=1; m2=1; v2=4
l2d.gp.u(m1,v1,m2,v2)
}
