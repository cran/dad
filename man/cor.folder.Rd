\name{cor.folder}
\alias{cor.folder}
\alias{corf}
\title{
  Correlation matrices of a folder of data sets
}
\description{
  Computes the correlation matrices of the elements of an object of class \code{folder}.
}
\usage{
cor.folder(x, use = "everything", method = "pearson")
}
\arguments{
  \item{x}{
     an object of class \code{\link{folder}} that is a list of data frames with the same column names.
}
  \item{use}{
     an optional character string giving a method for computing covariances in the presence of missing values. This must be (an abbreviation of) one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (see \code{\link{var}}).
}
  \item{method}{
     a character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
}
}
\details{
% This function can only be applied to a folder of data frames with the same column names. If \code{attr(x,"same.cols") = FALSE}, there is an error.
 It uses \code{\link{cor}} to compute the variance matrix of the numeric columns of each element of the folder. If some columns of the data frames are not numeric, there is a warning, and the variances are computed on the numeric columns only.
}
\value{
 A list whose elements are the correlation matrices of the elements of the folder.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folder}} to create an object is of class \code{folder}.
\code{\link{mean.folder}}, \code{\link{var.folder}}, \code{\link{skewness.folder}}, \code{\link{kurtosis.folder}} for other statistics for \code{folder} objects.
}
\examples{
# First example: iris (Fisher)               
data(iris)
iris.fold <- as.folder(iris, "Species")
iris.cor <- cor.folder(iris.fold)
print(iris.cor)

# Second example: roses
data(roses)
roses.fold <- as.folder(roses, "rose")
roses.cor <- cor.folder(roses.fold)
print(roses.cor)
}
