\name{ddhellingerpar}
\alias{ddhellingerpar}
\title{
     Distance between discrete probability distributions given the probabilities on their common support
}
\description{
    Hellinger (or Matusita) distance between two discrete probability distributions on the same support (which can be a Cartesian product of \eqn{q} sets) , given the probabilities of the states (which are \eqn{q}-tuples) of the support.
}
\usage{
ddhellingerpar(p1, p2)
}
\arguments{
  \item{p1}{
       array (or table) the dimension of which is \eqn{q}. The first probability distribution on the support.
}
  \item{p2}{
       array (or table) the dimension of which is \eqn{q}. The second probability distribution on the support.
}
}
\details{
    The Hellinger distance between two discrete distributions \eqn{p_1} and \eqn{p_2} is given by:
 \eqn{\sqrt{ \sum_x{(\sqrt{p_1(x)} - \sqrt{p_2(x)})^2}} }
    
    Notice that some authors divide this expression by \eqn{\sqrt{2}}.
}
\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}
\seealso{
   \code{\link{ddhellinger}}: Hellinger distance between two estimated discrete distributions, given samples.
   
   Other distances: \code{\link{ddchisqsympar}}, \code{\link{ddjeffreyspar}}, \code{\link{ddjensenpar}}, \code{\link{ddlppar}}.

}
\references{
    Deza, M.M. and Deza E. (2013). Encyclopedia of distances. Springer.
}
\examples{
# Example 1
p1 <- array(c(1/2, 1/2), dimnames = list(c("a", "b"))) 
p2 <- array(c(1/4, 3/4), dimnames = list(c("a", "b"))) 
ddhellingerpar(p1, p2)

# Example 2
x1 <- data.frame(x = factor(c("A", "A", "A", "B", "B", "B")),
                 y = factor(c("a", "a", "a", "b", "b", "b")))                 
x2 <- data.frame(x = factor(c("A", "A", "A", "B", "B")),
                 y = factor(c("a", "a", "b", "a", "b")))
p1 <- table(x1)/nrow(x1)                 
p2 <- table(x2)/nrow(x2)
ddhellingerpar(p1, p2)
}
