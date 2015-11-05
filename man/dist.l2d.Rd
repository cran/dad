\name{dist.l2d}
\alias{dist.l2d}
\alias{dist.l2d.gp}
\alias{dist.l2d.gp.u}
\alias{dist.l2d.gs}
\alias{dist.l2d.gs.u}
\alias{dist.l2d.kgw}
\alias{dist.l2d.kgw.u}
\alias{dist.l2d.kga}
\alias{dist.l2d.kga.u}
\title{
     Distance between probability densities
}
\description{
    Functions computing the \eqn{L^2} distance between two \eqn{p}-dimensional probability densities, say \eqn{f_1} and \eqn{f_2}, from the \eqn{L^2} inner product (see Details).
}
\usage{
dist.l2d.gp(mean1, var1, mean2, var2, check = FALSE)
dist.l2d.gp.u(mean1, var1, mean2, var2, check = FALSE)
dist.l2d.gs(x1, x2, check = FALSE)
dist.l2d.gs.u(x1, x2, check = FALSE)
dist.l2d.kgw(x1, varw1, x2, varw2, check = FALSE)
dist.l2d.kgw.u(x1, varw1, x2, varw2, check = FALSE)
dist.l2d.kga(x1, x2, check = FALSE)
dist.l2d.kga.u(x1, x2, check = FALSE)
}
\arguments{
  \item{mean1, mean2}{
       means of the probability densities.
}
  \item{var1, var2}{
       variances (\eqn{p}=1) or covariance matrices (\eqn{p}>1) of the probability densities.
}
  \item{x1, x2}{
       the samples from the probability densities.
}
  \item{varw1, varw2}{
       the bandwidths when the densities are estimated by the kernel method.
}
  \item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the variances (\eqn{p}=1) are not zero  or if the covariance matrices (\eqn{p}>1) are not degenerated. 
}
}
\details{
    The functions ``\code{dist.l2d...}'' compute the distance between \eqn{f_1} and \eqn{f_2} from the formula
 \deqn{||f_1 - f_2||^2 = ||f_1||^2 + ||f_2||^2 - 2 <f_1, f_2>.}
 The functions whose names end by ``\code{.u}'' apply to unidimensional densities and the others apply to \eqn{p}- dimensional densities with \eqn{p>1}.
    For some information about the method used to compute the \eqn{L^2} inner product or about the arguments, see the corresponding function, that is for \code{dist.l2d.gp}, see \code{\link{l2d.gp}} and so on: 

    \code{dist.l2d.gp.u}, see \code{\link{l2d.gp.u}} 

    \code{dist.l2d.gs}, see \code{\link{l2d.gs}} 
    
    \code{dist.l2d.gs.u}, see \code{\link{l2d.gs.u}} 
    
    \code{dist.l2d.kgw}, see \code{\link{l2d.kgw}} 
    
    \code{dist.l2d.kgw.u}, see \code{\link{l2d.kgw.u}}  
    
    \code{dist.l2d.kga} see, \code{\link{l2d.kga}}  
    
    \code{dist.l2d.kga.u}, see \code{\link{l2d.kga.u}} .
}
    
\value{
    Returns the \eqn{L^2} distance between the two densities.
}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}
\seealso{
   \code{\link{mat.dist.l2d}} in order to compute pairwise distances between several densities.
}
\examples{
u1=c(1,1,1);
v1=matrix(c(4,0,0,0,16,0,0,0,25),ncol=3);
u2=c(0,1,0);
v2=matrix(c(1,0,0,0,1,0,0,0,1),ncol=3);
dist.l2d.gp(u1,v1,u2,v2)
}
