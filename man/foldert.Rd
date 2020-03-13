\name{foldert}
\alias{foldert}
\title{
  Folder of data sets among time
}
\description{
  Creates an object of class \code{"foldert"} (called foldert below), that is a list of data frames, each of them corresponding to a time of observation. These data sets are on the same variables. They can be on the same individuals or not.
}
\usage{
foldert(x1, x2 = NULL, ..., times = NULL, cols.select = "intersect", rows.select = "")
%foldert(x1, x2 = NULL, ..., times = NULL, cols.select = "", rows.select = "", 
%same.rows = (rows.select \%in\% c("union", "intersect")))
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
     data frame. Must be provided if \code{x1} is a data frame. Omitted if x1 is a list of data frames.
}
  \item{...}{
     optional. One or several data frames when \code{x1} is a data frame. These supplementary data frames are added to the list of data frames constituting the returned foldert.
}
  \item{times}{
     Vector of the ``times'' of observations. It can be either numeric, or an ordered factor or an object of class \code{"Date"}, \code{"POSIXlt"} or \code{"POSIXct"}. If omitted, it is \code{1:N} where \code{N} is the number of data frame arguments (if \code{x1} is a data frame) or the length of \code{x1} (if it is a list).
     
     So there is an order relationship between these times.
}
  \item{cols.select}{
     string or character vector. Gives the method used to choose the column names of the data frames of the foldert. This argument can be:
     \describe{
     \item{\code{"intersect"}}{(default) the column names of the data frames in the foldert are the intersection of the column names of all the data frames given as arguments.}
     \item{\code{"union"}}{the column names of the data frames in the foldert are the union of the column names of all the data frames given as arguments. When necessary, the rows of the returned data frames are completed by NA.}
     }
     If \code{cols.select} is a character vector, it gives the column names selected in the data frames given as arguments. The corresponding columns constitute the columns of the elements of the returned foldert. Notice that when a column name is not present in all data frames (given as arguments), the data are completed by NA.
}
     \item{rows.select}{
     string. Gives the method used to choose the row names of the data frames of the foldert. This argument can be:
     \describe{
     \item{\code{""}}{(default) the data frames of the foldert have the same rows as those which were passed as arguments.
     }
     \item{\code{"intersect"}}{the row names of the data frames in the foldert are the intersection of the row names of all the data frames given as arguments.}
     \item{\code{"union"}}{the row names of the data frames in the foldert are the union of the row names of all the data frames given as arguments. When necessary, the columns of the data frames returned are completed by \code{NA}.}
     }
}
%  \item{same.rows}{
%     logical. If \code{TRUE}, the elements of the returned foldert have the same rows. That means that the same individuals are present in every data sets. This allows to consider the evolution of each individual among time. If \code{FALSE}, every rows of this foldert are different, and their names are made unique by adding the name of the data frame to the row names. In this case, The individuals of the data sets are assumed to be all different. Or, at least, the user does not mind if they are the same or not.
     
%     If omitted, \code{same.rows} is \code{TRUE} if \code{rows.select} is \code{"union"} or \code{"intersect"}, and \code{FALSE} otherwise.
%}
}
\value{
 Returns an object of class \code{"foldert"}, that is a list of data frames. The elements of this list are ordered according to time.
}
\details{
 The class \code{"foldert"} has an attribute \code{attr(,"times")} (the \code{times} argument, when provided) and a logical attributes %\code{attr(,"same.cols")} and 
 \code{attr(,"same.rows")}.
 
 The data frames in the returned foldert all have the same column names. That means that the same variables are observed in every data sets.
 
 If the \code{rows.select} argument is \code{"union"} or \code{"intersect"}, the elements of the returned foldert have the same rows. That means that the same individuals are present in every data sets. This allows to consider the evolution of each individual among time.
 
 If \code{rows.select} is \code{""}, every rows of this foldert are different, and the row names are made unique by adding the name of the data frame to the row names. In this case, The individuals of the data sets are assumed to be all different. Or, at least, the user does not mind if they are the same or not.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{is.foldert}} to test if an object is of class \code{foldert}.
\code{\link{as.foldert.data.frame}}: build an object of class \code{foldert} from a data frame.
\code{\link{as.foldert.array}}: build an object of class \code{foldert} from a \eqn{3d}-array.
}
\examples{
x <- data.frame(xyz = rep(c("A", "B", "C"), each = 2),
                xy = letters[1:6],
                x1 = rnorm(6),
                x2 = rnorm(6, 2, 1),
                row.names = paste0("i", 1:6),
                stringsAsFactors = TRUE)
y <- data.frame(xyz = c("A", "A", "B", "C"),
                xy = c("a", "b", "a", "c"),
                y1 = rnorm(4, 4, 2),
                row.names = c(paste0("i", c(1, 2, 4, 6))),
                stringsAsFactors = TRUE)
z <- data.frame(xyz = c("A", "B", "C"),
                z1 = rnorm(3),
                row.names = c("i1", "i2", "i5"),
                stringsAsFactors = TRUE)

# Columns selected by the user
ftc. <- foldert(x, y, z, cols.select = c("xyz", "x1", "y1", "z1"))
print(ftc.)

# cols.select = "union": all the variables (columns) of each data frame are kept
ftcun <- foldert(x, y, z, cols.select = "union")
print(ftcun)

# cols.select = "intersect": only variables common to all data frames
ftcint <- foldert(x, y, z, cols.select = "intersect")
print(ftcint)

# rows.select = "": the rows of the data frames are unchanged
# and the rownames are made unique
ftr. <- foldert(x, y, z, rows.select = "")
print(ftr.)

# rows.select = "union": all the individuals (rows) of each data frame are kept
ftrun <- foldert(x, y, z, rows.select = "union")
print(ftrun)

# rows.select = "intersect": only individuals common to all data frames
ftrint <- foldert(x, y, z, rows.select = "intersect")
print(ftrint)

# Define the times (times argument)
ftimes <- foldert(x, y, z, times = as.Date(c("2018-03-01", "2018-04-01", "2018-05-01")))
print(ftimes)
}
