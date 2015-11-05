\name{meanf}
\alias{meanf}
\title{
  Means of a folder of data sets
}
\description{
  Computes the means by column of the elements of an object of class \code{folder}.
}
\usage{
meanf(fold, na.rm = FALSE)
}
\arguments{
  \item{fold}{
     an object of class \code{\link{folder}}. The attribute \code{attr(,"same.cols")} must be \code{TRUE} (otherwise, there is an error).
}
  \item{na.rm}{
     logical. Should missing values (including NaN) be omitted from the calculations? (see \code{\link{mean}} or \code{\link{colMeans}})
}
}
\details{
 This function can only be applied to a folder of data frames with the same column names. If \code{attr(fold,"same.cols") = FALSE}, there is an error.
 
 It uses \code{\link{colMeans}} to compute the mean by numeric column of each element of the folder. If some columns of the data frames are not numeric, there is a warning, and the means are computed on the numeric columns only.
}
\value{
 A list whose elements are the mean by column of the elements of the folder.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard

Maintainer: Rachid Boumaza <rachid.boumaza@agrocampus-ouest.fr>
}

\seealso{
\code{\link{folder}} to create an object is of class \code{folder}.
\code{\link{varf}}, \code{\link{corf}}, \code{\link{skewnessf}}, \code{\link{kurtosisf}} for other statistics for \code{folder} objects.
}
\examples{
# First example: iris (Fisher)               
data(iris)
iris.fold <- df2folder(iris, "Species")
iris.means <- meanf(iris.fold)
print(iris.means)

# Second example: roses
data(roses)
roses.fold <- df2folder(roses, "Lot")
roses.means <- meanf(roses.fold)
print(roses.means)
}
