\name{l2dpar}
\alias{l2dpar}
%\alias{l2d.gp}
%\alias{l2d.gp.u}
\title{
     \eqn{L^2} inner product of Gaussian densities given their parameters
}
\description{
    \eqn{L^2} inner product of multivariate (\eqn{p > 1}) or univariate (\eqn{p = 1}) Gaussian densities, given their parameters (mean vectors and covariance matrices if the densities are multivariate, or means and variances if univariate).
}
\usage{
l2dpar(mean1, var1, mean2, var2, check = FALSE)
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
    Computes the inner product of two Gaussian densities, equal to: 

    \deqn{(2\pi)^{-p/2} det(var1 + var2)^{-1/2} exp(-(1/2) t(mean1 - mean2) (var1 + var2)^{-1} (mean1 - mean2))}

If \eqn{p = 1} the means and variances are numbers, the formula is the same ignoring the following operators: t (transpose of a matrix or vector) and det (determinant of a square matrix).
}
\value{
    The \eqn{L^2} inner product between two Gaussian densities.
    
    Be careful! If \code{check = FALSE} and one covariance matrix is degenerated (multivariate case) or one variance is zero (univariate case), the result returned must not be considered.
}
\references{
   M. Wand and M. Jones (1995). Kernel Smoothing. Chapman and Hall, London.
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

