\name{l2dpar}
\alias{l2dpar}
\alias{l2d.gp}
\alias{l2d.gp.u}
\title{
     \eqn{L^2} inner product of Gaussian densities
}
\description{
    Computes the \eqn{L^2} inner product of multivariate or univariate (dimension: \eqn{p \geq 1}) Gaussian densities, given their parameters (mean vectors and variance matrices if they are multivariate, means and variances if univariate).
}
\usage{
l2dpar(mean1, var1, mean2, var2, check = FALSE)
}
\arguments{
  \item{mean1}{
       \eqn{p}-length numeric vector: the mean of the first Gaussian density.
}
  \item{var1}{
       \eqn{p \times p}-symmetric numeric matrix (in the case of multivariate densities) or numeric (univariate): the covariance matrix (if multivariate) or the variance (if univariate) of the first Gaussian density.
}
  \item{mean2}{
       \eqn{p}-length numeric vector: the mean of the second Gaussian density.
}
  \item{var2}{
       \eqn{p \times p}-symmetric numerical matrix or numeric: the covariance matrix or variance of the second Gaussian density.
}
  \item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the covariance matrices are not degenerated, before computing the inner product. 
}
}
\details{
    Computes the inner product of the two \eqn{p}-dimensional Gaussian densities, equal to: 
    
    \itemize{
    \item In the case of multivariate densities: \deqn{(2\pi)^{-p/2} |var1+var2|^{-1/2} \exp(-\frac{1}{2}(mean1-mean2)'(var1+var2)^{-1}(mean1-mean2))}{%
          (2 pi)^{-p/2} |var1+var2|^{-1/2} exp(-(1/2)(mean1-mean2)'(var1+var2)^{-1}(mean1-mean2))}
    \item In the case of univariate densities: \deqn{(2\pi)^{-1/2} (var1+var2)^{-1/2} \exp(-\frac{1}{2}\frac{(mean1-mean2)^2}{var1+var2})}{%
          (2 pi)^{-1/2} (var1+var2)^{-1/2} exp(-1/2 (mean1-mean2)^2/(var1+var2))}
    }
}                                                                                                      
\value{
    Returns the \eqn{L^2} inner product of the two Gaussian densities.
    Be careful! If \code{check = FALSE} and one variance matrix is degenerated (or one variance is zero if the densities are univariate), the result returned must not be considered.
}
\references{
   Boumaza, R. (1998). Analyse en composantes principales de distributions gaussiennes multidimensionnelles. Revue de Statistique Appliquée, XLVI (2), 5-20.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \link{l2d} for parametrically estimated Gaussian densities or nonparametrically estimated densities, given samples;
}
\examples{
m1 <- c(1,1)
v1 <- matrix(c(4,1,1,9),ncol = 2)
m2 <- c(0,1)
v2 <- matrix(c(1,0,0,1),ncol = 2)
l2dpar(m1,v1,m2,v2)
}

