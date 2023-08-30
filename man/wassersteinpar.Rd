\name{wassersteinpar}
\alias{wassersteinpar}
\title{
     2-Wasserstein distance between Gaussian densities given their parameters
}
\description{
    The 2-Wasserstein distance between two multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) Gaussian densities given their parameters (mean vectors and covariance matrices if the densities are multivariate, or means and variances if univariate) (see Details).
}
\usage{
wassersteinpar(mean1, var1, mean2, var2, check = FALSE)
}
\arguments{
  \item{mean1}{
       \eqn{p}-length numeric vector: the mean of the first Gaussian density.
}
  \item{var1}{
       \eqn{p} x \eqn{p} symmetric numeric matrix (\eqn{p} > 1) or numeric (\eqn{p} = 1): the covariance matrix (\eqn{p} > 1) or the variance (\eqn{p} = 1) of the first Gaussian density.
}
  \item{mean2}{
       \eqn{p}-length numeric vector: the mean of the second Gaussian density.
}
  \item{var2}{
       \eqn{p} x \eqn{p} symmetric numeric matrix (\eqn{p} > 1) or numeric (\eqn{p} = 1): the covariance matrix (\eqn{p} > 1) or the variance (\eqn{p} = 1) of the second Gaussian density.
}
  \item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the covariance matrices are not degenerate (multivariate case) or if the variances are not zero (univariate case).
}
}
\details{
    The mean vectors (\eqn{m1} and \eqn{m2}) and variance matrices (\eqn{v1} and \eqn{v2}) given as arguments (\code{mean1}, \code{mean2}, \code{var1} and \code{var2}) are used to compute the 2-Wasserstein distance between the two Gaussian densities, equal to: 

    \deqn{(||m1-m2||_2^2 + trace((v1+v2) - 2*(v2^{1/2} v1 v2^{1/2})^{1/2}))^{1/2}}

    If \eqn{p = 1}:

    \deqn{((m1-m2)^2 + v1 + v2 - 2*(v1*v2)^{1/2})^{1/2}}

    }
\value{
    The 2-Wasserstein distance between two Gaussian densities.

    Be careful! If \code{check = FALSE} and one covariance matrix is degenerated (multivariate case) or one variance is zero (univariate case), the result returned must not be considered.
}
\references{
    Peterson, A., Mueller, H.G (2016). Functional Data Analysis for Density Functions by Transformation to a Hilbert Space. The annals of Statistics, 44 (1), 183-218. DOI: 10.1214/15-AOS1363
    
    Dowson, D.C., Ladau, B.V. (1982). The Fréchet Distance between Multivariate Normal Distributions. Journal of Multivariate Analysis, 12, 450-455.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \link{wasserstein}: 2-Wasserstein distance between Gaussian densities estimated from samples.
}
\examples{
m1 <- c(1,1)
v1 <- matrix(c(4,1,1,9),ncol = 2)
m2 <- c(0,1)
v2 <- matrix(c(1,0,0,1),ncol = 2)
wassersteinpar(m1,v1,m2,v2)
}

