\name{as.data.frame.foldert}
\alias{as.data.frame.foldert}
%\alias{as.data.frame}
\title{
  foldert to data frame
}
\description{
  Builds a data frame from an object of class \code{foldert}.
}
\usage{
\method{as.data.frame}{foldert}(x, row.names = NULL, optional = FALSE, ..., group.name = "time")
}
\arguments{
  \item{x}{
     object of class \code{\link{foldert}} with the same row names. An object of class \code{foldert} is a list of data frames with the same column names, each of them corresponding to a time of observation.
}
  \item{row.names, optional}{
     for consistency with \code{\link{as.data.frame}}. \code{as.data.frame.foldert} does not take them into account.
}
  \item{...}{
     further arguments passed to or from other methods.
}
  \item{group.name}{
     the name of the grouping variable. It is the name of the last column of the returned data frame.
     
     As the observations are indexed by time, the default value is \code{group.name = "time"}.
}
}

\value{
 \code{as.data.frame.foldert} returns a data frame.
}

\details{\code{as.data.frame.foldert} uses \code{\link{as.data.frame.folder}}.
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
print(ftflor)
dfflor <- as.data.frame(ftflor)
summary(dfflor)
}                                                      
