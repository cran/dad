\name{mean.folder}
\alias{mean.folder}
\alias{meanf}
\title{
  Means of a folder of data sets
}
\description{
  Computes the means by column of the elements of an object of class \code{folder}.
}
\usage{
\method{mean}{folder}(x, ..., na.rm = FALSE)
}
\arguments{
  \item{x}{
     an object of class \code{\link{folder}}. The attribute \code{attr(,"same.cols")} must be \code{TRUE} (otherwise, there is an error).
}
  \item{...}{
     further arguments passed to or from other methods.
}
  \item{na.rm}{
     logical. Should missing values (including NaN) be omitted from the calculations? (see \code{\link{mean}} or \code{\link{colMeans}})
}
}
\details{
 This function can only be applied to a folder of data frames with the same column names. If \code{attr(x,"same.cols") = FALSE}, there is an error.
 
 It uses \code{\link{colMeans}} to compute the mean by numeric column of each element of the folder. If some columns of the data frames are not numeric, there is a warning, and the means are computed on the numeric columns only.
}
\value{
 A list whose elements are the mean by column of the elements of the folder.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folder}} to create an object of class \code{folder}.
\code{\link{var.folder}}, \code{\link{cor.folder}}, \code{\link{skewness.folder}}, \code{\link{kurtosis.folder}} for other statistics for \code{folder} objects.
}
\examples{
# First example: iris (Fisher)               
data(iris)
iris.fold <- as.folder(iris, "Species")
iris.means <- mean(iris.fold)
print(iris.means)

# Second example: roses
data(roses)
roses.fold <- as.folder(roses, "rose")
roses.means <- mean(roses.fold)
print(roses.means)
}
