\name{as.foldert.array}
\alias{as.foldert.array}
\title{
  Data frame to foldert
}
\description{
  Builds an object of class \code{\link{foldert}} from a \eqn{3d}-array.
}
\usage{
\method{as.foldert}{array}(x, ind = 1, var = 2, time = 3, ...)
}
\arguments{
  \item{x}{
     a \eqn{3d}-array.
}
  \item{ind, var, time}{
     three distinct integers among 1, 2 and 3.
     
     \code{ind} gives the dimension of the observations,
     \code{var} gives the dimension of the variables
     and \code{ind} gives the dimension of the times.
}
  \item{...}{
     further arguments passed to or from other methods.
}
}
\value{
 an object \code{ft} of class \code{\link{foldert}} that is a list of data frames, each of them corresponding to a time of observation; these data frames have the same column names.
 
 They necessarily have the same row names (\code{attr(ft, "same.rows")=TRUE}).
 The \code{"times"} attribute of \code{ft}: \code{attr(ft, "times")} is a numeric vector, an ordered factor or an object of class \code{Date}, and contains the values nf the dimension of \code{x} given by \code{time} argument.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{foldert}}: objects of class \code{foldert}.

\code{\link{as.foldert.data.frame}}: build an object of class \code{foldert} from a data frame.
}
\examples{
x <- array(c(rep(0, 5), rep(0, 5), rep(0, 5),
             rnorm(5, 2, 1), rnorm(5, 3, 2), rnorm(5, -2, 0.5),
             rnorm(5, 4, 1), rnorm(5, 5, 3), rnorm(5, -3, 1)),
           dim = c(5, 3, 3),
           dimnames = list(1:5, c("z1", "z2", "z3"), c("t1", "t2", "t3")))
# The individuals which were observed are on the 1st dimension,
# the variables are on the 2nd dimension and the times are on the 3rd dimension.
ft <- as.foldert(x, ind = 1, var = 2, time = 3)
}