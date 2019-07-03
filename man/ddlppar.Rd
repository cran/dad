\name{ddlppar}
\alias{ddlppar}
\title{
     Distance between discrete probability distributions given the probabilities on their common support
}
\description{
    \eqn{L^p} distance between two discrete probability distributions on the same support (which can be a Cartesian product of \eqn{q} sets) , given the probabilities of the states (which are \eqn{q}-tuples) of the support.
}
\usage{
ddlppar(p1, p2, p = 1)
}
\arguments{
  \item{p1}{
       array (or table) the dimension of which is \eqn{q}. The first probability distribution on the support.
}
  \item{p2}{
       array (or table) the dimension of which is \eqn{q}. The second probability distribution on the support.
}
  \item{p}{
       integer. Parameter of the distance.
}
}
\details{
    The \eqn{L^p} distance \eqn{||p_1 - p_2||} between two discrete distributions \eqn{p_1} and \eqn{p_2} is given by the formula:
 \deqn{||p_1 - p_2||^p = \sum_x{|p_1(x)-p_2(x)|^p}}
 
    If \eqn{p=1}, it is the variational distance.

    If \eqn{p=2}, it is the Patrick-Fisher distance.
}
\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{ddlp}}: \eqn{L^p} distance between two estimated discrete distributions, given samples.
   
   Other distances: \code{\link{ddchisqsympar}}, \code{\link{ddhellingerpar}}, \code{\link{ddjeffreyspar}}, \code{\link{ddjensenpar}}.
}
\references{
    Deza, M.M. and Deza E. (2013). Encyclopedia of distances. Springer.
}
\examples{
# Example 1
p1 <- array(c(1/2, 1/2), dimnames = list(c("a", "b"))) 
p2 <- array(c(1/4, 3/4), dimnames = list(c("a", "b"))) 
ddlppar(p1, p2)
ddlppar(p1, p2, p=2)

# Example 2
x1 <- data.frame(x = factor(c("A", "A", "A", "B", "B", "B")),
                 y = factor(c("a", "a", "a", "b", "b", "b")))                 
x2 <- data.frame(x = factor(c("A", "A", "A", "B", "B")),
                 y = factor(c("a", "a", "b", "a", "b")))
p1 <- table(x1)/nrow(x1)                 
p2 <- table(x2)/nrow(x2)
ddlppar(p1, p2)
}
