\name{l2d.gs}
\alias{l2d.gs}
\title{
    \eqn{L^2} inner product of multivariate Gaussian densities
}
\description{
    Computes the \eqn{L^2} inner product of multivariate Gaussian densities, parametrically estimated from samples.
}
\usage{
l2d.gs(x1, x2, check=FALSE)
}
\arguments{
  \item{x1}{
       a matrix or data frame of \eqn{n_1}{n1} rows (observations) and \eqn{p} columns (variables), with \eqn{n_1 >= p}{n1 >= p}.
}
  \item{x2}{
       a matrix or data frame of \eqn{n_2}{n2} rows and \eqn{p} columns, with \eqn{n_2 >= p}{n2 >= p}.
}
  \item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the covariance matrices of the samples are not degenerate, before computing the inner product. 
}
}
\details{
    Computes the mean vectors (\eqn{m1} and \eqn{m2}) and the variance matrices (\eqn{v1} and \eqn{v2}) of the two samples, then uses \link{l2d.gp} to compute the inner product equal to:
     
    \deqn{(2\pi)^{-p/2} |v1+v2|^{-1/2} \exp(-\frac{1}{2}(m1-m2)'(v1+v2)^{-1}(m1-m2))}{%
        (2 pi)^{-p/2} |v1+v2|^{-1/2} exp(-(1/2)(m1-m2)'(v1+v2)^{-1}(m1-m2))}
}                                                                                                      
\value{
    Returns the \eqn{L^2} inner product of the two Gaussian densities associated to the samples.
    Be careful! If \code{check=FALSE} and one variance matrix is degenerated, the result returned must not be considered.
}
\references{
   Boumaza, R. (1998). Analyse en composantes principales de distributions gaussiennes multidimensionnelles. Revue de Statistique Appliquée, XLVI (2), 5-20.
}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}
\seealso{
   \link{l2d.gs.u} for univariate densities; \link{l2d.gp} for Gaussian densities, given their parameters;
   \link{l2d.kga} and \link{l2d.kgw} for estimated densities using the Gaussian kernel method.
}
\examples{
require(MASS)
m1=c(0,0)
v1=matrix(c(1,0,0,1),ncol=2) 
m2=c(0,1)
v2=matrix(c(4,1,1,9),ncol=2)
x1=mvrnorm(n=3,mu=m1,Sigma=v1)
x2=mvrnorm(n=5,mu=m2,Sigma=v2)
l2d.gs(x1,x2)
}
