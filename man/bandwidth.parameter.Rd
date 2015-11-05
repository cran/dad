\name{bandwidth.parameter}
\alias{bandwidth.parameter}
\title{
    Parameter of the normal reference rule
    }
\description{
    Computation of the parameter of the normal reference rule in order to estimate the (matrix) bandwidth. 
     }
\usage{
bandwidth.parameter(p, n)
}
\arguments{
  \item{p}{
       sample dimension.
}
  \item{n}{
       sample size.
}
}
\details{
     The parameter is equal to:
      \deqn{h =(\frac{4}{n(p+2)})^{\frac{1}{p+4}}}{\{4/(n*(p+2))\}^(1/(p+4))}
      It is based on the minimisation of the asymptotic mean integrated square error in density estimation when using the Gaussian kernel method (Wand and Jones, 1995). 
}

\value{
    Returns the value required by the functions \code{\link{fpcad}}, \code{\link{fmdsd}}, \code{\link{fdiscd.misclass}} and \code{\link{fdiscd.predict}} when their argument \code{windowh} is set to \code{NULL}. 
}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}
    
\references{    
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.

Wand, M. P., Jones, M. C. (1995). Kernel Smoothing. Boca Raton, FL: Chapman and Hall.    
}
\examples{
    # Sample size :
    n = 20
    # Number of variables :
    p = 3
    bandwidth.parameter(p, n)
}
