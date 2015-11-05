\name{varf}
\alias{varf}
\title{
  Variance matrices of a folder of data sets
}
\description{
  Computes the variance matrices of the elements of an object of class \code{folder}.
}
\usage{
varf(fold, use = "everything")
}
\arguments{
  \item{fold}{
     an object of class \code{\link{folder}}. The attribute \code{attr(,"same.cols")} must be \code{TRUE} (otherwise, there is an error).
}
  \item{use}{
     an optional character string giving a method for computing covariances in the presence of missing values. This must be (an abbreviation of) one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (see \code{\link{var}}).
}
}
\details{
 This function can only be applied to a folder of data frames with the same column names. If \code{attr(fold,"same.cols") = FALSE}, there is an error.
 
 It uses \code{\link{var}} to compute the variance matrix of the numeric columns of each element of the folder. If some columns of the data frames are not numeric, there is a warning, and the variances are computed on the numeric columns only.
}
\value{
 A list whose elements are the variance matrices of the elements of the folder.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard

Maintainer: Rachid Boumaza <rachid.boumaza@agrocampus-ouest.fr>
}

\seealso{
\code{\link{folder}} to create an object is of class \code{folder}.
\code{\link{meanf}}, \code{\link{corf}}, \code{\link{skewnessf}}, \code{\link{kurtosisf}} for other statistics for \code{folder} objects.
}
\examples{
# First example: iris (Fisher)               
data(iris)
iris.fold <- df2folder(iris, "Species")
iris.vars <- varf(iris.fold)
print(iris.vars)

# Second example: roses
data(roses)
roses.fold <- df2folder(roses, "Lot")
roses.vars <- varf(roses.fold)
print(roses.vars)
}
