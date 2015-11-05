\name{l2d.kga.u}
\alias{l2d.kga.u}
\title{
     \eqn{L^2} inner product of estimated probability densities
}
\description{
    Computes the \eqn{L^2} inner product of univariate probability densities, using the Gaussian kernel estimation method.
}
\usage{
l2d.kga.u(x1, x2, check=FALSE)
}
\arguments{
  \item{x1, x2}{
       vectors. The observations of each sample.
}
  \item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the variances are not zero, before computing the inner product. 
}
}
\details{
    Estimates the variances (\eqn{v_1} and \eqn{v_2}) from the two samples, then uses the Gaussian kernel method and the normal reference rule matrix bandwidth to compute their inner product. 
    
    The normal reference rule matrix bandwidth is \deqn{h_1 v_1^{1/2}} where
    \deqn{h_1=(\frac{4}{3 n_1})^\frac{1}{5}}{h_1=(4/(3 n_1))^{1/5}}
    for the first density. Idem for the second density after making the necessary changes.
}
\value{
    Returns the \eqn{L^2} inner product of the two probability densities.
    Be careful! If \code{check=FALSE} and one variance is zero, the result returned can not be considered.
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.

   Wand, M., Jones, M. (1995). Kernel smoothing. Chapman and Hall/CRC, London.

   Yousfi, S., Boumaza R., Aissani, D., Adjabi, S. (2014). Optimal bandwith matrices in functional principal component analysis of density functions. Journal of Statistical Computational and Simulation, 85 (11), 2315-2330.

}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}
\seealso{
   \link{l2d.kga} for univariate densities; \link{l2d.kgw.u} for estimated densities using the Gaussian kernel method, given the banwidths; \link{l2d.gp.u} and \link{l2d.gs.u} for Gaussian densities.
}
\examples{
x1=rnorm(n=3,m=0,sd=1)
x2=rnorm(n=5,m=1,sd=2)
l2d.kga.u(x1,x2)
}
