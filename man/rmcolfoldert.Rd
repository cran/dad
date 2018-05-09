\name{rmcol.foldert}
\alias{rmcol.foldert}
\title{
  Remove cols in all elements of a foldert
}
\description{
  Remove some columns in all data frames of a foldert.
}
\usage{
rmcol.foldert(object, name)
}
\arguments{
  \item{object}{
     object of class \code{\link{foldert}} that is a list of data frames with the same column names, each of them corresponding to a time of observation.
}
  \item{name}{
     character vector. The names of the columns to be removed in each data frame of the foldert.
}
}

\value{
 A foldert with the same number of elements as \code{object}. Its \eqn{k^{th}} element is a data frame, and its columns are the columns of \code{object[[k]]}, except those given by \code{name}.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{foldert}}: object of class \code{foldert}.

\code{\link{getcol.foldert}}: select columns in all elements of a foldert.


\code{\link{getrow.foldert}}: get rows in all elements of a foldert.

\code{\link{rmrow.foldert}}: remove rows in all elements of a foldert.
}
\examples{
data(floribundity)

ft0 <- foldert(floribundity, cols.select = "union", rows.select = "union")
ft0
rmcol.foldert(ft0, c("area"))
}                                                      
