\name{summary.foldert}
\alias{summary.foldert}
\title{
  Summarize a foldert
}
\description{
  Summarize an object of class \code{foldert}.
}
\usage{
\method{summary}{foldert}(object, ...)
}
\arguments{
  \item{object}{
     object of class \code{\link{foldert}} that is a list of data frames organised according to time.
}
  \item{...}{
     further arguments passed to or from other methods.
}
}

\value{
 A list, each element of it contains the summary of the corresponding element of \code{object}.
 This list has three attributes \code{attr(, "times")}, \code{attr(, "same.cols")} and \code{attr(, "same.rows")}.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{foldert}}: object of class \code{foldert}.
%\code{\link{as.foldert.data.frame}}: build an object of class \code{foldert} from a data frame.
}
\examples{
# 1st example
data(floribundity)
ftflor <- foldert(floribundity, cols.select = "union", rows.select = "union")
summary(ftflor)
}                                                      
