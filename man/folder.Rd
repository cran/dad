\name{folder}
\alias{folder}
\title{
  Folder of data sets
}
\description{
  Creates an object of class folder, that is a list of data frames which have the same column names.
}
\usage{
folder(x1, x2 = NULL, ..., cols.select = "intersect")
}
\arguments{
  \item{x1}{
     data frame or list of data frames.
     \itemize{
     \item If \code{x1} is a data frame, \code{x2} must be provided.
     \item If \code{x1} is a list of data frames, its elements are the datasets of the folder. In this case, there is no \code{x2} argument.
     }
}
  \item{x2}{
     data frame. Must be provided if \code{x1} is a data frame.
}
  \item{\dots}{
     optional. One or several data frames. When \code{x1} and \code{x2} are data frames, these are the other data frames.
}
  \item{cols.select}{
     string. Gives the method used to choose the column names of the data frames of the folder. This argument can be:
     \describe{
     \item{\code{"intersect"}}{the data frames of the folder have the column names which are the intersection of the column names of all the date frames given as arguments.}
     \item{\code{"union"}}{the data frames of the folder have the column names which are the union of the column names of all the date frames given as arguments. When necessary, the rows of the data frames returned are completed by NA.}
     }
}
}
\value{
 Returns an object of class \code{folder}, that is a list of data frames.
}
\details{
 The class \code{folder} has 2 logical attributes \code{attr(,"same.cols")} and \code{attr(,"same.rows")} respectively equal to TRUE and FALSE in the current version.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{is.folder}} to test if an object is of class \code{folder}.
\code{\link{folderh}} to build a folder of two data frames with a hierarchic relation between the two datasets.
}
\examples{
# First example              
x1 <- data.frame(x = rnorm(10), y = 1:10)
x2 <- data.frame(x = rnorm(10), z = runif(10, 1, 10))
f1 <- folder(x1, x2)
print(f1)

f2 <- folder(x1, x2, cols.select = "union")
print(f2)

#Second example
data(iris)
iris.set <- iris[iris$Species == "setosa", 1:4]
iris.ver <- iris[iris$Species == "versicolor", 1:4]
iris.vir <- iris[iris$Species == "virginica", 1:4]
irisf1 <- folder(iris.set, iris.ver, iris.vir)
print(irisf1)

listofdf <- list(df1 = iris.set,df2 = iris.ver,df3 = iris.vir)
irisf2 <- folder(listofdf,x2 = NULL)
print(irisf2)
}
