\name{getcol.foldert}
\alias{getcol.foldert}
\title{
  Select columns in all elements of a foldert
}
\description{
  Select columns in all data frames of a foldert.
}
\usage{
getcol.foldert(object, name)
}
\arguments{
  \item{object}{
     object of class \code{\link{foldert}} that is a list of data frames with the same column names, each of them corresponding to a time of observation.
}
  \item{name}{
     character vector. The names of the columns to be selected in each data frame of the foldert.
}
}

\value{
 A foldert with the same number of elements as \code{object}. Its \eqn{k^{th}} element is a data frame, and its columns are the columns of \code{object[[k]]} given by \code{name}.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{foldert}}: object of class \code{foldert}.

\code{\link{rmcol.foldert}}: remove columns in all elements of a foldert.

\code{\link{getrow.foldert}}: select rows in all elements of a foldert.

\code{\link{rmrow.foldert}}: remove rows in all elements of a foldert.
}
\examples{
data(floribundity)

ft0 <- foldert(floribundity, cols.select = "union")
getcol.foldert(ft0, c("rose", "variety"))
}                                                      
