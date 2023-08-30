\name{as.data.frame.folder}
\alias{as.data.frame.folder}
%\alias{folder2df}
%\alias{as.data.frame}
\title{
  Folder to data frame
}
\description{
  Builds a data frame from an object of class \code{folder}.
}
\usage{
\method{as.data.frame}{folder}(x, row.names = NULL, optional = FALSE, ..., group.name = "group")
}
\arguments{
  \item{x}{
     object of class \code{\link{folder}} that is a list of data frames with the same column names.
}
  \item{row.names, optional}{
     for consistency with \code{\link{as.data.frame}}. \code{as.data.frame.folder} does not take them into account.
}
  \item{...}{
     further arguments passed to or from other methods.
}
  \item{group.name}{
     the name of the grouping variable. It is the name of the last column of the returned data frame.
}
}

\value{
 \code{as.data.frame.folder} returns a data frame.
}

\details{The data frame is simply obtained by row binding the data frames of the folder and adding a factor (as last column). The name of this column is given by \code{group.name} argument. The levels of this factor are the names of the elements of the folder.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folder}}: object of class \code{folder}.
\code{\link{as.folder.data.frame}}: build an object of class \code{folder} from a data frame.
}
\examples{
data(iris)

iris.fold <- as.folder(iris, "Species")
print(iris.fold)

iris.df <- as.data.frame(iris.fold)
print(iris.df)
}                                                      
