\name{folder}
\alias{folder}
\title{
  Folder of data sets
}
\description{
  Creates an object of class \code{"folder"} (called folder below), that is a list of data frames with the same column names. Thus, these data sets are on the same variables. They can be on the same individuals or not.
}
\usage{
folder(x1, x2 = NULL, ..., cols.select = "intersect", rows.select = "")
}
\arguments{
  \item{x1}{
     data frame (can also be a tibble) or list of data frames.
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
     \item{\code{"intersect"}}{(default) the column names of the data frames in the folder are the intersection of the column names of all the data frames given as arguments.}
     \item{\code{"union"}}{the column names of the data frames in the folder are the union of the column names of all the data frames given as arguments. When necessary, the rows of the returned data frames are completed by NA.}
     If \code{cols.select} is a character vector, it gives the column names selected in the data frames given as arguments. The corresponding columns constitute the columns of the elements of the returned folder. Notice that when a column name is not present in all data frames (given as arguments), the data are completed by NA.
     }
}
     \item{rows.select}{
     string. Gives the method used to choose the row names of the data frames of the folder. This argument can be:
     \describe{
     \item{\code{""}}{(default) the data frames of the folder have the same rows as those which were passed as arguments.
     }
     \item{\code{"intersect"}}{the row names of the data frames in the folder are the intersection of the row names of all the data frames given as arguments.}
     \item{\code{"union"}}{the row names of the data frames in the folder are the union of the row names of all the data frames given as arguments. When necessary, the columns of the data frames returned are completed by \code{NA}.}
     }
}
}
\value{
 Returns an object of class \code{"folder"}, that is a list of data frames.
}
\details{
 The class \code{folder} has a logical attributes \code{attr(,"same.rows")}.
 
 The data frames in the returned folder all have the same column names. That means that the same variables are observed in every data sets.
 
 If the \code{rows.select} argument is \code{"union"} or \code{"intersect"}, the elements of the returned folder have the same rows. That means that the same individuals are present in every data sets. This allows to consider the evolution of each individual among time.
 
 If \code{rows.select} is \code{""}, every rows of this folder are different, and the row names are made unique by adding the name of the data frame to the row names. In this case, The individuals of the data sets are assumed to be all different. Or, at least, the user does not mind if they are the same or not.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{is.folder}} to test if an object is of class \code{folder}.
\code{\link{folderh}} to build a folder of several data frames with a hierarchic relation between each pair of consecutive data frames.
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
