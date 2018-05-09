\name{rmrow.folder}
\alias{rmrow.folder}
\title{
  Remove rows in all elements of a folder
}
\description{
  Remove some rows in all data frames of a folder.
}
\usage{
rmrow.folder(object, name)
}
\arguments{
  \item{object}{
     object of class \code{\link{folder}} that is a list of data frames with the same column names.
}
  \item{name}{
     character vector. The names of the rows to be removed in each data frame of the folder.
}
}

\value{
 A folder with the same number of elements as \code{object}. Its \eqn{k^{th}} element is a data frame, and its rows are the rows of \code{object[[k]]}, except those given by \code{name}.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folder}}: object of class \code{folder}.

\code{\link{getrow.folder}}: select rows in all elements of a folder.

\code{\link{getcol.folder}}: select columns in all elements of a folder.

\code{\link{rmcol.folder}}: remove columns in all elements of a folder.
}
\examples{
data(iris)

iris.fold <- as.folder(iris, "Species")
rmrow.folder(iris.fold, as.character(seq(1, 150, by = 2)))
}                                                      
