\name{l2d.kga}
\alias{l2d.kga}
\title{
    \eqn{L^2} inner product of estimated probability densities
}
\description{
    Computes the \eqn{L^2} inner product of multivariate probability densities, using the Gaussian kernel estimation method.
}
\usage{
l2d.kga(x1, x2, check=FALSE)
}
\arguments{
  \item{x1}{
       a matrix or data frame of \eqn{n_1}{n1} rows (observations) and \eqn{p} columns (variables), with \eqn{n1 >= p}.
}
  \item{x2}{
       a matrix or data frame of \eqn{n_2}{n2} rows and \eqn{p} columns, with \eqn{n2 >= p}.
}
  \item{check}{
       logical. When \code{TRUE} (the default is \code{FALSE}) the function checks if the covariance matrices are not degenerate, before computing the inner product. 
}
}
\details{
    Estimates the variance matrices (\eqn{v_1} and \eqn{v_2}) from the two samples, then uses the Gaussian kernel method and the normal reference rule matrix bandwidth to compute their inner product. 
    
    The normal reference rule matrix bandwidth is \deqn{h_1 v_1^{1/2}} where
    \deqn{h_1=(\frac{4}{n_1(p+2)})^\frac{1}{p+4}}{h_1=(4/(n_1(p+2)))^{1/(p+4)}}
    for the first density.  Idem for the second density after making the necessary changes.
}
\value{
    Returns the \eqn{L^2} inner product of the two probability densities.
    Be careful! If \code{check=FALSE} and one variance matrix is degenerated, the result returned must not be considered.
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.

   Wand, M., Jones, M. (1995). Kernel smoothing. Chapman and Hall/CRC, London.

   Yousfi, S., Boumaza, R., Aissani, D., Adjabi, S. (2014). Optimal bandwith matrices in functional principal component analysis of density functions. Journal of Statistical Computational and Simulation, 85 (11), 2315-2330.

}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}
\seealso{
   \link{l2d.kga.u} for univariate densities; \link{l2d.kgw} for estimated densities using the Gaussian kernel method, given the banwidth matrices; \link{l2d.gp} and \link{l2d.gs} for Gaussian densities.
}
\examples{
data(roses)
xA = roses[roses$Lot=="A",c("Sha","Den","Sym")]
xB = roses[roses$Lot=="B",c("Sha","Den","Sym")]
l2d.kga(xA,xB)
}
