\name{distl2dpar}
\alias{distl2dpar}
\alias{dist.l2d.gp}
\alias{dist.l2d.gp.u}
\title{
     \eqn{L^2} distance between Gaussian densities given their parameters
}
\description{
    \eqn{L^2} distance between two multivariate (\eqn{p > 1}) or univariate (dimension: \eqn{p = 1}) Gaussian densities, given their parameters (mean vectors and covariance matrices if the densities are multivariate, or means and variances if univariate).
}
\usage{
distl2dpar(mean1, var1, mean2, var2, check = FALSE)
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
    The function \code{distl2dpar} computes the distance between two densities, say \eqn{f_1} and \eqn{f_2}, from the formula:
    
 \deqn{||f_1 - f_2||^2 = <f_1, f_1> + <f_2, f_2> - 2 <f_1, f_2>}.
 
    For some information about the method used to compute the \eqn{L^2} inner product or about the arguments, see \code{\link{l2dpar}}.
}
    
\value{
    The \eqn{L^2} distance between the two densities.
    
    Be careful! If \code{check = FALSE} and one variance matrix is degenerated (or one variance is zero if the densities are univariate), the result returned must not be considered.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{matdistl2d}} in order to compute pairwise distances between several densities.
}
\examples{
u1 <- c(1,1,1);
v1 <- matrix(c(4,0,0,0,16,0,0,0,25),ncol = 3);
u2 <- c(0,1,0);
v2 <- matrix(c(1,0,0,0,1,0,0,0,1),ncol = 3);
distl2dpar(u1,v1,u2,v2)
}
