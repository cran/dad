\name{hellingerpar}
\alias{hellingerpar}
\title{
     Hellinger distance between Gaussian densities given their parameters
}
\description{
    Hellinger distance between two multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) Gaussian densities given their parameters (mean vectors and covariance matrices if the densities are multivariate, or means and variances if univariate) (see Details).
}
\usage{
hellingerpar(mean1, var1, mean2, var2, check = FALSE)
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
    The mean vectors (\eqn{m1} and \eqn{m2}) and variance matrices (\eqn{v1} and \eqn{v2}) given as arguments (\code{mean1}, \code{mean2}, \code{var1} and \code{var2}) are used to compute the Hellinger distance between the two Gaussian densities, equal to: 

    \deqn{( 2 (1 -  2^{p/2} det(v1 v2)^{1/4} det(v1 + v2)^{-1/2}  exp((-1/4) t(m1-m2) (v1+v2)^{-1} (m1-m2)) ))^{1/2}}

    If \eqn{p = 1} the means and variances are numbers, the formula is the same ignoring the following operators: t (transpose of a matrix or vector) and det (determinant of a square matrix).
    }
\value{
    The Hellinger distance between two Gaussian densities.

    Be careful! If \code{check = FALSE} and one covariance matrix is degenerated (multivariate case) or one variance is zero (univariate case), the result returned must not be considered.
}
\references{
    McLachlan, G.J. (1992). Discriminant analysis and statistical pattern recognition. John Wiley & Sons, New York .
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \link{hellinger}: Hellinger distance between Gaussian densities estimated from samples.
}
\examples{
m1 <- c(1,1)
v1 <- matrix(c(4,1,1,9),ncol = 2)
m2 <- c(0,1)
v2 <- matrix(c(1,0,0,1),ncol = 2)
hellingerpar(m1,v1,m2,v2)
}

