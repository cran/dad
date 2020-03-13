\name{ddlp}
\alias{ddlp}
\title{
     Distance between probability distributions of discrete variables given samples   
}
\description{
    \eqn{L^p} distance between two multivariate (\eqn{q > 1}) or univariate (\eqn{q = 1}) discrete probability distributions, estimated from samples.
}
\usage{
ddlp(x1, x2, p = 1)
}
\arguments{
  \item{x1, x2}{
      vectors or data frames of \eqn{q} columns (can also be tibbles).
      
      If they are data frames and have not the same column names, there is a warning.
}
  \item{p}{
       integer. Parameter of the distance.
}
}
\details{
    Let \eqn{p_1} and \eqn{p_2} denote the estimated probability distributions of the discrete samples \eqn{x_1} and \eqn{x_2}. The \eqn{L^p} distance between the discrete probability distributions of the samples are computed using the \code{\link{ddlppar}} function.
 }
    
\value{
    The distance between the two discrete probability distributions.
}
\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{ddlppar}}: \eqn{L^p} distance between two discrete distributions, given the probabilities on their common support.
   
   Other distances: \code{\link{ddchisqsym}}, \code{\link{ddhellinger}}, \code{\link{ddjeffreys}}, \code{\link{ddjensen}}.
   
}
\references{
    Deza, M.M. and Deza E. (2013). Encyclopedia of distances. Springer.
}
\examples{
# Example 1
x1 <- c("A", "A", "B", "B")
x2 <- c("A", "A", "A", "B", "B")
ddlp(x1, x2)
ddlp(x1, x2, p = 2)

# Example 2
x1 <- data.frame(x = factor(c("A", "A", "A", "B", "B", "B")),
                 y = factor(c("a", "a", "a", "b", "b", "b")))                 
x2 <- data.frame(x = factor(c("A", "A", "A", "B", "B")),
                 y = factor(c("a", "a", "b", "a", "b")))
ddlp(x1, x2)
}
