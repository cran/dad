\name{as.folder.data.frame}
\alias{as.folder.data.frame}
\alias{df2folder}
%\alias{as.folder.default}
\title{
  Data frame to folder
}
\description{
  Builds an object of class \code{folder} from a data frame.
}
\usage{
\method{as.folder}{data.frame}(x, groups = tail(colnames(x), 1), ...)
}
\arguments{
  \item{x}{
     data frame.
}
  \item{groups}{
     string. The name of the column of x containing the grouping variable. \code{x[, groups]} must be a factor, otherwise, there is an error.
     
     If omitted, the last column of \code{x} is used as grouping variable.
}
  \item{...}{
     further arguments passed to or from other methods.
}
}
\value{
 \code{as.folder.data.frame} returns an object of class \code{\link{folder}} that is a list of data frames with the same column names.
 
 Each element of the folder contains the data corresponding to one level of \code{x[, groups]}.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folder}}: objects of class \code{folder}.
\code{\link{as.data.frame.folder}}: build a data frame from an object of class \code{folder}.
\code{\link{as.folder.folderh}}: build an object of class \code{folder} from an object of class \code{folderh}.
}
\examples{
# First example: iris (Fisher)               
data(iris)
iris.fold <- as.folder(iris, "Species")
print(iris.fold)

# Second example: roses
data(roses)
roses.fold <- as.folder(roses, "rose")
print(roses.fold)
}
