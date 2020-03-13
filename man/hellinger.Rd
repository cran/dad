\name{hellinger}
\alias{hellinger}
\title{
     Hellinger distance between Gaussian densities
}
\description{
    Hellinger distance between two multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) Gaussian densities (see Details).
}
\usage{
hellinger(x1, x2, check = FALSE)
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
    The Hellinger distance between the two Gaussian densities is computed by using the \code{\link{hellingerpar}} function and the density parameters estimated from samples.
}
\value{
    Returns the \eqn{Hellinger} distance between the two probability densities.

    Be careful! If \code{check = FALSE} and one smoothing bandwidth matrix is degenerate, the result returned can not be considered.
}
\references{
    McLachlan, G.J. (1992). Discriminant analysis and statistical pattern recognition. John Wiley & Sons, New York .
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \link{hellingerpar}: Hellinger distance between Gaussian densities, given their parameters.
}
\examples{
require(MASS)
m1 <- c(0,0)
v1 <- matrix(c(1,0,0,1),ncol = 2) 
m2 <- c(0,1)
v2 <- matrix(c(4,1,1,9),ncol = 2)
x1 <- mvrnorm(n = 3,mu = m1,Sigma = v1)
x2 <- mvrnorm(n = 5, mu = m2, Sigma = v2)
hellinger(x1, x2)
}
