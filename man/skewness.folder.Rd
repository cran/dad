\name{skewness.folder}
\alias{skewness.folder}
%\alias{skewnessf}
\title{
  Skewness coefficients of a folder of data sets
}
\description{
  Computes the skewness coefficient by column of the elements of an object of class \code{folder}.
}
\usage{
skewness.folder(x, na.rm = FALSE, type = 3)
}
\arguments{
  \item{x}{
     an object of class \code{\link{folder}} that is a list of data frames with the same column names.
}
  \item{na.rm}{
     logical. Should missing values be omitted from the calculations? (see \code{\link[e1071]{skewness}})
}
  \item{type}{
     an integer between 1 and 3 (see \code{\link[e1071]{skewness}}).
}
}
\details{
% This function can only be applied to a folder of data frames with the same column names. If \code{attr(x,"same.cols") = FALSE}, there is an error.
 It uses \code{\link[e1071]{skewness}} to compute the mean by numeric column of each element of the folder. If some columns of the data frames are not numeric, there is a warning, and the means are computed on the numeric columns only.
}
\value{
 A list whose elements are the skewness coefficients by column of the elements of the folder.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folder}} to create an object is of class \code{folder}.
\code{\link{mean.folder}}, \code{\link{var.folder}}, \code{\link{cor.folder}}, \code{\link{kurtosis.folder}} for other statistics for \code{folder} objects.
}
\examples{
# First example: iris (Fisher)               
data(iris)
iris.fold <- as.folder(iris, "Species")
iris.skewness <- skewness.folder(iris.fold)
print(iris.skewness)

# Second example: roses
data(roses)
roses.fold <- as.folder(roses, "rose")
roses.skewness <- skewness.folder(roses.fold)
print(roses.skewness)
}
