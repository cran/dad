\name{wasserstein}
\alias{wasserstein}
\title{
     2-Wasserstein distance between Gaussian densities
}
\description{
    The 2-Wasserstein distance between two multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) Gaussian densities (see Details).
}
\usage{
wasserstein(x1, x2, check = FALSE)
}
\arguments{
  \item{x1}{
       a matrix or data frame of \eqn{n_1}{n1} rows (observations) and \eqn{p} columns (variables) (can also be a tibble) or a vector of length \eqn{n_1}{n1}.
}
  \item{x2}{
       matrix or data frame (or tibble) of \eqn{n_2}{n2} rows and \eqn{p} columns or vector of length \eqn{n_2}{n2}.
}
  \item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the covariance matrices are not degenerate (multivariate case) or if the variances are not zero (univariate case).
}
}
\details{
    The Wasserstein distance between the two Gaussian densities is computed by using the \code{\link{wassersteinpar}} function and the density parameters estimated from samples.
}
\value{
    Returns the 2-\eqn{Wasserstein} distance between the two probability densities.

    Be careful! If \code{check = FALSE} and one smoothing bandwidth matrix is degenerate, the result returned can not be considered.
}
\references{
    Peterson, A., Mueller, H.G. (2016). Functional Data Analysis for Density Functions by Transformation to a Hilbert Space. The annals of Statistics, 44 (1), 183-218. DOI: 10.1214/15-AOS1363
    
    Dowson, D.C., Ladau, B.V. (1982). The Fréchet Distance between Multivariate Normal Distributions. Journal of Multivariate Analysis, 12, 450-455.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \link{wassersteinpar}: 2-Wasserstein distance between Gaussian densities, given their parameters.
}
\examples{
require(MASS)
m1 <- c(0,0)
v1 <- matrix(c(1,0,0,1),ncol = 2) 
m2 <- c(0,1)
v2 <- matrix(c(4,1,1,9),ncol = 2)
x1 <- mvrnorm(n = 3,mu = m1,Sigma = v1)
x2 <- mvrnorm(n = 5, mu = m2, Sigma = v2)
wasserstein(x1, x2)
}
