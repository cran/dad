\name{kurtosis.folder}
\alias{kurtosis.folder}
%\alias{kurtosisf}
\title{
  Kurtosis coefficients of a folder of data sets
}
\description{
  Computes the kurtosis coefficient by column of the elements of an object of class \code{folder}.
}
\usage{
kurtosis.folder(x, na.rm = FALSE, type = 3)
}
\arguments{
  \item{x}{
     an object of class \code{\link{folder}}.% The attribute \code{attr(,"same.cols")} must be \code{TRUE} (otherwise, there is an error).
}
  \item{na.rm}{
     logical. Should missing values be omitted from the calculations? (see \code{\link{kurtosis}})
}
  \item{type}{
     an integer between 1 and 3 (see \code{\link{kurtosis}}).
}
}
\details{
% This function can only be applied to a folder of data frames with the same column names. If \code{attr(x,"same.cols") = FALSE}, there is an error.
 It uses \code{\link{kurtosis}} to compute the mean by numeric column of each element of the folder. If some columns of the data frames are not numeric, there is a warning, and the means are computed on the numeric columns only.
}
\value{
 A list whose elements are the kurtosis coefficients by column of the elements of the folder.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folder}} to create an object is of class \code{folder}.
\code{\link{mean.folder}}, \code{\link{var.folder}}, \code{\link{cor.folder}}, \code{\link{skewness.folder}} for other statistics for \code{folder} objects.
}
\examples{
# First example: iris (Fisher)               
data(iris)
iris.fold <- as.folder(iris, "Species")
iris.kurtosis <- kurtosis.folder(iris.fold)
print(iris.kurtosis)

# Second example: roses
data(roses)
roses.fold <- as.folder(roses, "rose")
roses.kurtosis <- kurtosis.folder(roses.fold)
print(roses.kurtosis)
}
