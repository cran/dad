\name{summary.folder}
\alias{summary.folder}
\title{
  Summarize a folder
}
\description{
  Summarize an object of class \code{folder}.
}
\usage{
\method{summary}{folder}(object, ...)
}
\arguments{
  \item{object}{
     object of class \code{\link{folder}} that is a list of data frames. These data frames have the same column names.
}
  \item{...}{
     further arguments passed to or from other methods.
}
}

\value{
% An object of class \code{summary.folder}.
% 
% An object of class \code{summary.folder} is a
 A list, each element of it containing the summary of the corresponding element of \code{object}.
 This list has two attributes \code{attr(, "same.cols")} and \code{attr(, "same.rows")}.
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
