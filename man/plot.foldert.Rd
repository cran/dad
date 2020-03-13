\name{plot.foldert}
\alias{plot.foldert}

\title{
    Plotting data of a foldert
}
\description{
    Applies to an object of class \code{\link{foldert}} (called foldert below) that is a list.
    Plots the longitudinal evolution of a numeric variable for every individuals.
}
\usage{
\method{plot}{foldert}(x, which, na.inter = TRUE, type = "l", ylim = NULL, ylab = which,
                       main = "", ...)
}

\arguments{
  \item{x}{
       object of class \code{\link{foldert}} that is a list of data frames with the same column names, each of them corresponding to a time of observation.
}
  \item{which}{
       character. Name of a column of the data frames of \code{x}.
       It gives the name of the variable to be plotted.
       
       For each element \code{x[[k]]} of \code{x}, \code{x[[k]]} must be numeric.
       Otherwise, there is an error
}
  \item{na.inter}{
       logical. If \code{TRUE} (default), for each individual, the missing values are deleted before plotting its evolution.
       If \code{FALSE}, the line corresponding to each individual is interrupted if there is a missing value, as for \code{\link{matplot}}.
}
  \item{type}{
       character string (length 1 vector) or vector of 1-character strings (default \code{"l"}) indicating the type of plot for each of the individuals followed among time, that is for each line of the data frames in the foldert.
       For further information about this argument, see \code{\link{matplot}}.
}
  \item{ylim}{
       ranges of y axis. \code{xlim} is as in \code{\link{matplot}}.
       See details.
}
  \item{ylab}{
       a label for the \code{y} axis. Default: the name of the plotted variable (\code{which} argument).
}
  \item{main}{
       an overall title for the plot: see \code{\link{title}}.
}
  \item{...}{
      optional arguments to \code{plot} methods.
}
}
\details{
   Internally, \code{plot.foldert} builds a matrix \code{mdata} containing the data of the variable given by \code{which} argument.
   The element \code{mdata[ind, t]} of this matrix is the value of the variable \code{which} for the individual \code{ind}: \code{x[[t]][ind, which]}.
   
   If the \code{ylim} argument is omitted, the range of \code{y} axis is given by \code{range(mdata, na.rm = TRUE)*c(0, 1.2)}.
}
\references{
    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{foldert}}: object of class \code{foldert}.
\code{\link{as.foldert.data.frame}}: build an object of class \code{foldert} from a data frame.
\code{\link{as.foldert.array}}: build an object of class \code{foldert} from a \eqn{3d}-array.
}
\examples{
data(floribundity)
ftflor <- foldert(floribundity, cols.select = "union", rows.select = "union")
plot(ftflor, which = "nflowers", ylab = "Number of flowers per plant",
     main = "Floribundity of rosebushes, 2010, Angers (France)")
}
