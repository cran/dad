\name{l2d.gp}
\alias{l2d.gp}
\title{
     \eqn{L^2} inner product of multivariate Gaussian densities
}
\description{
    Computes the \eqn{L^2} inner product of multivariate (dimension: \eqn{p}>1) Gaussian densities, given their parameters (means and variance matrices).
}
\usage{
l2d.gp(mean1, var1, mean2, var2, check=FALSE)
}
\arguments{
  \item{mean1}{
       \eqn{p}-length numeric vector: the mean of the first Gaussian density.
}
  \item{var1}{
       \eqn{p \times p}-symmetric numeric matrix: the covariance matrix of the first Gaussian density.
}
  \item{mean2}{
       \eqn{p}-length numeric vector: the mean of the second Gaussian density.
}
  \item{var2}{
       \eqn{p \times p}-symmetric numerical matrix: the covariance matrix of the second Gaussian density.
}
  \item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the covariance matrices are not degenerated, before computing the inner product. 
}
}
\details{
    Computes the inner product of the two \eqn{p}-dimensional Gaussian densities, equal to: 
    
    \deqn{(2\pi)^{-p/2} |var1+var2|^{-1/2} \exp(-\frac{1}{2}(mean1-mean2)'(var1+var2)^{-1}(mean1-mean2))}{%
          (2 pi)^{-p/2} |var1+var2|^{-1/2} exp(-(1/2)(mean1-mean2)'(var1+var2)^{-1}(mean1-mean2))}
}                                                                                                      
\value{
    Returns the \eqn{L^2} inner product of the two Gaussian densities.
    Be careful! If \code{check=FALSE} and one variance matrix is degenerated, the result returned must not be considered.
}
\references{
   Boumaza, R. (1998). Analyse en composantes principales de distributions gaussiennes multidimensionnelles. Revue de Statistique Appliquée, XLVI (2), 5-20.
}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}
\seealso{
   \link{l2d.gp.u} for univariate densities; \link{l2d.gs} for parametrically estimated Gaussian densities, given samples;
   \link{l2d.kga} and \link{l2d.kgw} for estimated densities using the Gaussian kernel method.
}
\examples{
m1=c(1,1)
v1=matrix(c(4,1,1,9),ncol=2)
m2=c(0,1)
v2=matrix(c(1,0,0,1),ncol=2)
l2d.gp(m1,v1,m2,v2)
}

