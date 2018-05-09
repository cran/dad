\name{getcol.folder}
\alias{getcol.folder}
\title{
  Select columns in all elements of a folder
}
\description{
  Select columns in all data frames of a folder.
}
\usage{
getcol.folder(object, name)
}
\arguments{
  \item{object}{
     object of class \code{\link{folder}} that is a list of data frames with the same column names.
}
  \item{name}{
     character vector. The names of the columns to be selected in each data frame of the folder.
}
}

\value{
 A folder with the same number of elements as \code{object}. Its \eqn{k^{th}} element is a data frame, and its columns are the columns of \code{object[[k]]} given by \code{name}.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folder}}: object of class \code{folder}.

\code{\link{rmcol.folder}}: remove columns in all elements of a folder.

\code{\link{getrow.folder}}: select rows in all elements of a folder.

\code{\link{rmrow.folder}}: remove rows in all elements of a folder.
}
\examples{
data(iris)

iris.fold <- as.folder(iris, "Species")
getcol.folder(iris.fold, "Sepal.Length")
getcol.folder(iris.fold, c("Petal.Length", "Petal.Width"))
}                                                      
