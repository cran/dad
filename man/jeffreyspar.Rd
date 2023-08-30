\name{jeffreyspar}
\alias{jeffreyspar}
\title{
     Jeffreys measure between Gaussian densities given their parameters
}
\description{
Jeffreys measure (or symmetrised Kullback-Leibler divergence) between two multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) Gaussian densities, given their parameters (mean vectors and covariance matrices if they are multivariate, means and variances if univariate) (see Details).
}
\usage{
jeffreyspar(mean1, var1, mean2, var2, check = FALSE)
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
    Let \eqn{m1} and \eqn{m2} the mean vectors, \eqn{v1} and \eqn{v2} the covariance matrices, Jeffreys measure of the two Gaussian densities is equal to:
    
    \deqn{(1/2) t(m1 - m2) (v1^{-1} + v2^{-1}) (m1 - m2) - (1/2) tr( (v1 - v2) (v1^{-1} - v2^{-1}) )}.
    
If \eqn{p = 1} the means and variances are numbers, the formula is the same ignoring the following operators: t (transpose of a matrix or vector) and tr (trace of a square matrix).
}                                                                                                      
\value{
    Jeffreys measure between two Gaussian densities.

    Be careful! If \code{check = FALSE} and one covariance matrix is degenerated (multivariate case) or one variance is zero (univariate case), the result returned must not be considered.
}
\references{
    McLachlan, G.J. (1992). Discriminant analysis and statistical pattern recognition. John Wiley & Sons, New York .
    
    Thabane, L., Safiul Haq, M. (1999). On Bayesian selection of the best population using the Kullback-Leibler divergence measure. Statistica     Neerlandica, 53(3): 342-360.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \link{jeffreys}: Jeffreys measure of two parametrically estimated Gaussian densities, given samples.
}
\examples{
m1 <- c(1,1)
v1 <- matrix(c(4,1,1,9),ncol = 2)
m2 <- c(0,1)
v2 <- matrix(c(1,0,0,1),ncol = 2)
jeffreyspar(m1,v1,m2,v2)
}

