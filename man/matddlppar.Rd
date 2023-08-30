\name{matddlppar}
\alias{matddlppar}
\title{
    Matrix of distances between discrete probability densities given the probabilities on their common support
}
\description{
    Computes the matrix of the \eqn{L^p} distances between several multivariate or univariate discrete probability distributions on the same support (which can be a Cartesian product of \eqn{q} sets), given the probabilities of the states (which are \eqn{q}-tuples) of the support.
}
\usage{
matddlppar(freq, p = 1)
}
\arguments{
  \item{freq}{
       list of arrays. Their \code{dim} attribute is a vector with length \eqn{q}, its elements containing the numbers of levels of the \eqn{sets}. Each array contains the probabilities of the discrete distribution on the same support.
}
  \item{p}{
       integer. Parameter of the distance.
}
}
\value{
    Positive symmetric matrix whose order is equal to the number of distributions, consisting of the pairwise \eqn{L^p} distances between these distributions. 
}
\references{
    Deza, M.M. and Deza E. (2013). Encyclopedia of distances. Springer.
}
\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{ddlppar}}.
  
   \code{\link{matddlp}} for discrete probability distributions which are estimated from samples.
}
