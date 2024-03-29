\name{plot.fpcat}
\alias{plot.fpcat}
%\alias{plot}

\title{
    Plotting scores of principal component analysis of density functions among time
}
\description{
    Applies to an object of class \code{"fpcat"} (see details of the  
    \code{\link{fpcat}} function). Plots the scores.
}
\usage{
\method{plot}{fpcat}(x, nscore=c(1, 2), main = "PCA of probability density functions",
    sub.title = NULL, ...)
}

\arguments{
  \item{x}{
       object of class \code{"fpcat"} (returned by \code{\link{fpcat}}).
}
  \item{nscore}{
       numeric or length 2 numeric vector. If it is a length 2 numeric vector (default), it contains the numbers of the score vectors to be plotted. If it is a single value, it is the number of the score which is plotted among time.

       Warning: The components of \code{nscore} cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{fpcat}} function.
}
  \item{main}{
       this argument to title has an useful default here.
}
  \item{sub.title}{
       string. Subtitle to be added to each graph.
}
  \item{...}{
      optional arguments to \code{plot} methods.
}
}
\details{
   Plots:
   \itemize{
   \item{if \code{nscore} is a length 2 vector (default): the principal scores returned by the \code{\link{fpcat}} function with arrows from the point corresponding to each time to the next one.}
   \item{if \code{nscore} is a single value, the principal scores among time with arrows from each time to the next one.}
   }
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{fpcat}; \link{print.fpcat}%; \link{interpret.fpcat}.
}
\examples{
times <- as.Date(c("2017-03-01", "2017-04-01", "2017-05-01", "2017-06-01"))
x1 <- data.frame(z1=rnorm(6,1,5), z2=rnorm(6,3,3))
x2 <- data.frame(z1=rnorm(6,4,6), z2=rnorm(6,5,2))
x3 <- data.frame(z1=rnorm(6,7,2), z2=rnorm(6,8,4))
x4 <- data.frame(z1=rnorm(6,9,3), z2=rnorm(6,10,2))
ft <- foldert(x1, x2, x3, x4, times = times, rows.select="intersect")
print(ft)
result <- fpcat(ft)
plot(result)
plot(result,  nscore = c(1, 2))
plot(result,  nscore = 1)
plot(result)
}
