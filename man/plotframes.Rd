\name{plotframes}
\alias{plotframes}

\title{
    Plotting of two sets of variables
}
\description{
    Plots a set of numeric variables vs. another set and prints the pairwise correlations. It uses the function \code{\link{xyplot}} of \code{lattice} package. 
}
\usage{
plotframes(x, y, font.size = 12, layout = NULL)
}

\arguments{
  \item{x}{
       data frame. Variables on x coordinates.
}
  \item{y}{
       data frame. Variables on y coordinates.
}
  \item{font.size}{
       integer. Size of the characters in the strips.% (See the \code{\link{xyplot}} function).
}
  \item{layout}{
       numeric vector of length 2 or 3 giving the number of columns, rows, and optionally pages of the lattice. If omitted, the graphs will be displayed on 3 lines and 3 columns, with a number of pages set to the required number.
}
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\examples{
require(MASS)
mx <- c(0,0)
vx <- matrix(c(1,0,0,1),ncol = 2) 
my <- c(0,1)
vy <- matrix(c(4,1,1,9),ncol = 2)
x <- as.data.frame(mvrnorm(n = 10, mu = mx, Sigma = vx))
y <- as.data.frame(mvrnorm(n = 10, mu = my, Sigma = vy))
colnames(x) <- c("x1", "x2")
colnames(y) <- c("y1", "y2")
plotframes(x, y)
}