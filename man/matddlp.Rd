\name{matddlp}
\alias{matddlp}
\title{
    Matrix of distances between discrete probability distributions given samples
}
\description{
    Computes the matrix of the \eqn{L^p} distances between several multivariate or univariate discrete probability distributions, estimated from samples.
}
\usage{
matddlp(x, p = 1)
}
\arguments{
  \item{x}{
       object of class \code{"folder"} containing the data. Its elements are data frames (one data frame per distribution) whose columns are factors.
}
  \item{p}{
       integer. Parameter of the distance.
}
}
\value{
    Positive symmetric matrix whose order is equal to the number of data frames (or distributions), consisting of the pairwise \eqn{L^p} distances between the distributions. 
}
\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{ddlp}}.
   
   \code{\link{matddlppar}} for discrete probability distributions, given the probabilities on the same support.
}
\references{
    Deza, M.M. and Deza E. (2013). Encyclopedia of distances. Springer.
}
\examples{
# Example 1
x1 <- data.frame(x = factor(c("A", "A", "B", "B")))
x2 <- data.frame(x = factor(c("A", "A", "A", "B", "B")))
x3 <- data.frame(x = factor(c("A", "A", "B", "B", "B", "B")))
xf <- folder(x1, x2, x3)
matddlp(xf)
matddlp(xf, p = 2)

# Example 2
x1 <- data.frame(x = factor(c("A", "A", "A", "B", "B", "B")),
                 y = factor(c("a", "a", "a", "b", "b", "b")))                 
x2 <- data.frame(x = factor(c("A", "A", "A", "B", "B")),
                 y = factor(c("a", "a", "b", "a", "b")))
x3 <- data.frame(x = factor(c("A", "A", "B", "B", "B", "B")),
                 y = factor(c("a", "b", "a", "b", "a", "b")))
xf <- folder(x1, x2, x3)
matddlp(xf, p = 1)
}
