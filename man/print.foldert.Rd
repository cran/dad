\name{print.foldert}
\alias{print.foldert}
%\alias{print}
\title{
    Printing an object of class \code{foldert}
}
\description{
    \code{print} function, applied to an object of class \code{"foldert"}, prints a foldert, as returned by \code{\link{foldert}} or \code{\link{as.foldert}} function.
}
\usage{
\method{print}{foldert}(x, ...)
}
\arguments{
  \item{x}{
     an object of class \code{\link{foldert}}.
}
  \item{...}{
      optional arguments to \code{print} methods.
}
}
\details{
    The foldert is printed. In any data frame \code{x[[t]]} of this foldert, if a row is entirely \code{NA} (which means that the corresponding individual was not observed at time \code{t}), this row are not printed.
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

ft <- foldert(floribundity, cols.select = "union", rows.select = "union")
print(ft)
}
