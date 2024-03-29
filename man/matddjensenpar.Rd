\name{matddjensenpar}
\alias{matddjensenpar}
\title{
    Matrix of divergences between discrete probability densities given the probabilities on their common support
}
\description{
    Computes the matrix of the Jensen-Shannon divergences between several multivariate or univariate discrete probability distributions on the same support (which can be a Cartesian product of \eqn{q} sets), given the probabilities of the states (which are \eqn{q}-tuples) of the support.
}
\usage{
matddjensenpar(freq)
}
\arguments{
  \item{freq}{
       list of arrays. Their \code{dim} attribute is a vector with length \eqn{q}, its elements containing the numbers of levels of the \eqn{sets}. Each array contains the probabilities of the discrete distribution on the same support.
}
}
\value{
    Positive symmetric matrix whose order is equal to the number of densities, consisting of the pairwise Jensen-Shannon divergences between the discrete probability densities. 
}
\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{ddjensenpar}}.
   
   \code{\link{matddjensen}} for discrete probability densities which are estimated from the data.
}
\references{
    Deza, M.M. and Deza E. (2013). Encyclopedia of distances. Springer.
    
}
