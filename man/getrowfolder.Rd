\name{getrow.folder}
\alias{getrow.folder}
\title{
  Select rows in all elements of a folder
}
\description{
  Select rows in all data frames of a folder.
}
\usage{
getrow.folder(object, name)
}
\arguments{
  \item{object}{
     object of class \code{\link{folder}} that is a list of data frames with the same column names.
}
  \item{name}{
     character vector. The names of the rows to be selected in each data frame of the folder.
}
}

\value{
 A folder with the same number of elements as \code{object}. Its \eqn{k^{th}} element is a data frame, and its rows are the rows of \code{object[[k]]} given by \code{name}.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folder}}: object of class \code{folder}.

\code{\link{rmrow.folder}}: remove rows in all elements of a folder.

\code{\link{getcol.folder}}: select rows in all elements of a folder.

\code{\link{rmcol.folder}}: remove rows in all elements of a folder.
}
\examples{
data(iris)

iris.fold <- as.folder(iris, "Species")
getrow.folder(iris.fold, c(1:5, 51:55, 101:105))
}                                                      
