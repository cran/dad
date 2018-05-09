\name{var.folder}
\alias{var.folder}
\alias{varf}
\title{
  Variance matrices of a folder of data sets
}
\description{
  Computes the variance matrices of the elements of an object of class \code{folder}.
}
\usage{
var.folder(x, na.rm = FALSE, use = "everything")
}
\arguments{
  \item{x}{
     an object of class \code{\link{folder}} that is a list of data frames with the same column names.
}
  \item{na.rm}{
     logical. Should missing values be removed? (see \code{\link{var}})
}
  \item{use}{
     an optional character string giving a method for computing covariances in the presence of missing values. This must be (an abbreviation of) one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (see \code{\link{var}}).
}
}
\details{
% This function can only be applied to a folder of data frames with the same column names. If \code{attr(x,"same.cols") = FALSE}, there is an error.
 It uses \code{\link{var}} to compute the variance matrix of the numeric columns of each element of the folder. If some columns of the data frames are not numeric, there is a warning, and the variances are computed on the numeric columns only.
}
\value{
 A list whose elements are the variance matrices of the elements of the folder.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folder}} to create an object is of class \code{folder}.
\code{\link{mean.folder}}, \code{\link{cor.folder}}, \code{\link{skewness.folder}}, \code{\link{kurtosis.folder}} for other statistics for \code{folder} objects.
}
\examples{
# First example: iris (Fisher)               
data(iris)
iris.fold <- as.folder(iris, "Species")
iris.vars <- var.folder(iris.fold)
print(iris.vars)

# Second example: roses
data(roses)
roses.fold <- as.folder(roses, "rose")
roses.vars <- var.folder(roses.fold)
print(roses.vars)
}
