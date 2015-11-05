\name{l2d}
\alias{l2d}
\alias{Inner product of probability density functions}
\title{
     Inner product of probability densities
}
\description{
    Functions computing the \eqn{L^2} inner product of two multivariate or univariate probability densities.
}
\details{
    The functions use parametrical or nonparametrical estimation methods:

    \itemize{
    \item Gaussian densities, given the parameters:
      \itemize{
      \item multivariate: \code{\link{l2d.gp}}
      \item univariate: \code{\link{l2d.gp.u}}
      }
    \item Parametrically estimated Gaussian densities, given samples:
      \itemize{
      \item multivariate: \code{\link{l2d.gs}}
      \item univariate: \code{\link{l2d.gs.u}}
      }
    \item Estimated densities using the Gaussian kernel method, with the normal reference rule bandwidth:
      \itemize{
      \item multivariate: \code{\link{l2d.kga}}
      \item univariate: \code{\link{l2d.kga.u}}
      }
    \item Estimated densities using the Gaussian kernel method, given the smoothing banwidths:
      \itemize{
      \item multivariate: \code{\link{l2d.kgw}}
      \item univariate: \code{\link{l2d.kgw.u}}
      }
    }
}
\author{
    Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard.
}

