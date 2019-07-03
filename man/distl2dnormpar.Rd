\name{distl2dnormpar}
\alias{distl2dnormpar}
\title{
     \eqn{L^2} distance between \eqn{L^2}-normed Gaussian densities given their parameters
}
\description{
    \eqn{L^2} distance between two multivariate (\eqn{p > 1}) or univariate (dimension: \eqn{p = 1}) \eqn{L^2}-normed Gaussian densities, given their parameters (mean vectors and covariance matrices if the densities are multivariate, or means and variances if univariate) where a \eqn{L^2}-normed probability density is the original probability density function divided by its \eqn{L^2}-norm.
}
\usage{
distl2dnormpar(mean1, var1, mean2, var2, check = FALSE)
}
\arguments{
  \item{mean1, mean2}{
       means of the probability densities.
}
  \item{var1, var2}{
       variances (\eqn{p} = 1) or covariance matrices (\eqn{p} > 1) of the probability densities.
}
  \item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the covariance matrices are not degenerate, before computing the inner product.
       
       If the variables are univariate, it checks if the variances are not zero.
}
}
\details{
    Given  densities \eqn{f_1} and \eqn{f_2}, the function \code{distl2dnormpar} computes the distance between the \eqn{L^2}-normed densities \eqn{f_1 / ||f_1||} and \eqn{f_2 / ||f_2||}:
    
 \deqn{2 - 2 <f_1, f_2> / (||f_1|| ||f_2||)}.
 
    For some information about the method used to compute the \eqn{L^2} inner product or about the arguments, see \code{\link{l2dpar}}; the norm \eqn{||f||} of the multivariate Gaussian density \eqn{f} is equal to \eqn{(4\pi)^{-p/4} det(var)^{-1/4}}.
}
    
\value{
    The \eqn{L^2} distance between the two \eqn{L^2}-normed Gaussian densities.
    
    Be careful! If \code{check = FALSE} and one variance matrix is degenerated (or one variance is zero if the densities are univariate), the result returned must not be considered.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{distl2dpar}} for the distance between two probability densities.
   
   \code{\link{matdistl2d}} in order to compute pairwise distances between several densities.
}
\examples{
u1 <- c(1,1,1);
v1 <- matrix(c(4,0,0,0,16,0,0,0,25),ncol = 3);
u2 <- c(0,1,0);
v2 <- matrix(c(1,0,0,0,1,0,0,0,1),ncol = 3);
distl2dnormpar(u1,v1,u2,v2)
}
