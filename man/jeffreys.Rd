\name{jeffreys}
\alias{jeffreys}
\title{
     Jeffreys measure between Gaussian densities
}
\description{
Jeffreys measure (or symmetrised Kullback-Leibler divergence) between two multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) Gaussian densities given samples (see Details).
}
\usage{
jeffreys(x1, x2, check = FALSE)
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
    The Jeffreys measure between the two Gaussian densities is computed by using the \code{\link{jeffreyspar}} function and the density parameters estimated from samples.
}
\value{
    Returns the Jeffrey's measure between the two probability densities.

    Be careful! If \code{check = FALSE} and one smoothing bandwidth matrix is degenerate, the result returned must not be considered.
}
\references{
    Thabane, L., Safiul Haq, M. (1999). On Bayesian selection of the best population using the Kullback-Leibler divergence measure. Statistica     Neerlandica, 53(3): 342-360.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \link{jeffreyspar}: Jeffreys measure between Gaussian densities, given their parameters.
}
\examples{
require(MASS)
m1 <- c(0,0)
v1 <- matrix(c(1,0,0,1),ncol = 2) 
m2 <- c(0,1)
v2 <- matrix(c(4,1,1,9),ncol = 2)
x1 <- mvrnorm(n = 3,mu = m1,Sigma = v1)
x2 <- mvrnorm(n = 5, mu = m2, Sigma = v2)
jeffreys(x1, x2)
}
