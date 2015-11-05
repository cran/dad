\name{kurtosisf}
\alias{kurtosisf}
\title{
  Kurtosis coefficients of a folder of data sets
}
\description{
  Computes the kurtosis coefficient by column of the elements of an object of class \code{folder}.
}
\usage{
kurtosisf(fold, na.rm = FALSE)
}
\arguments{
  \item{fold}{
     an object of class \code{\link{folder}}. The attribute \code{attr(,"same.cols")} must be \code{TRUE} (otherwise, there is an error).
}
  \item{na.rm}{
     logical. Should missing values be omitted from the calculations? (see \code{\link{kurtosis}})
}
}
\details{
 This function can only be applied to a folder of data frames with the same column names. If \code{attr(fold,"same.cols") = FALSE}, there is an error.
 
 It uses \code{\link{kurtosis}} to compute the mean by numeric column of each element of the folder. If some columns of the data frames are not numeric, there is a warning, and the means are computed on the numeric columns only.
}
\value{
 A list whose elements are the kurtosis coefficients by column of the elements of the folder.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard

Maintainer: Rachid Boumaza <rachid.boumaza@agrocampus-ouest.fr>
}

\seealso{
\code{\link{folder}} to create an object is of class \code{folder}.
\code{\link{meanf}}, \code{\link{varf}}, \code{\link{corf}}, \code{\link{kurtosisf}} for other statistics for \code{folder} objects.
}
\examples{
# First example: iris (Fisher)               
data(iris)
iris.fold <- df2folder(iris, "Species")
iris.kurtosis <- kurtosisf(iris.fold)
print(iris.kurtosis)

# Second example: roses
data(roses)
roses.fold <- df2folder(roses, "Lot")
roses.kurtosis <- kurtosisf(roses.fold)
print(roses.kurtosis)
}
