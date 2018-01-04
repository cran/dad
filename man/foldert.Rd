\name{foldert}
\alias{foldert}
\title{
  Folder of data sets among time
}
\description{
  Creates an object of class \code{"foldert"} (called foldert below), that is a list of data frames, each of them corresponding to a time of observation. These data sets can be on the same variables or individuals or not.
}
\usage{
foldert(x1, x2 = NULL, ..., times = NULL, cols.select = "", rows.select = "", 
same.rows = (rows.select \%in\% c("union", "intersect")))
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
     data frame. Must be provided if \code{x1} is a data frame. Omitted if x1 is a list of data frames.
}
  \item{...}{
     optional. One or several data frames when \code{x1} is a data frame. These supplementary data frames are added to the list of data frames constituting the returned foldert.
}
  \item{times}{
     Vector of the ``times'' of observations. It can be either numeric, or an ordered factor or an object of class "Date". If omitted, it is \code{1:N} where \code{N} is the number of data frame arguments (if \code{x1} is a data frame) or the length of \code{x1} (if it is a list). 
     So there is an order relationship between these times.
}
  \item{cols.select}{
     string or character vector. Gives the method used to choose the column names of the data frames of the foldert. This argument can be:
     \describe{
     \item{\code{""}}{(default) the columns names of the data frames in the foldert are the same as those of the data frames given as arguments: \code{x1} (if it is a list of data frames) or \code{x1}, \code{x2},\code{...} (if they are data frames).
     If all of these data frames have the same column names, then the \code{"same.cols"} attribute of the returned \code{"foldert"} is \code{TRUE}. Otherwise, it is \code{FALSE}.
     }
     \item{\code{"intersect"}}{the column names of the data frames in the foldert are the intersection of the column names of all the data frames given as arguments.}
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
     \item{\code{"union"}}{the row names of the data frames in the foldert are the union of the row names of all the data frames given as arguments. When necessary, the columns of the data frames returned are completed by NA.}
     }
}
  \item{same.rows}{
     logical. If \code{TRUE}, the elements of the returned foldert have the same rows. That means that the same individuals are present in every data sets. This allows to consider the evolution of each individual among time. If \code{FALSE}, every rows of this foldert are different, and their names are made unique by adding the name of the data frame to the row names. In this case, The individuals of the data sets are assumed to be all different. Or, at least, the user does not mind if they are the same or not.
     
     If omitted, \code{same.rows} is \code{TRUE} if \code{rows.select} is \code{"union"} or \code{"intersect"}, and \code{FALSE} otherwise.
}
}
\value{
 Returns an object of class \code{"foldert"}, that is a list of data frames. The elements of this list are ordered according to time.
}
\details{
 The class \code{"foldert"} has an attribute \code{attr(,"times")} (the \code{times} argument, when provided) and 2 logical attributes \code{attr(,"same.cols")} and \code{attr(,"same.rows")}.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{is.foldert}} to test if an object is of class \code{foldert}.
%\code{\link{as.foldert}} to coerce a data frame into an object of class \code{"foldert"}.
}
\examples{
data(floribundity)

# Arguments with their default values
ft0 <- foldert(floribundity)
print(ft0)

# rows.select = "union", same.rows = TRUE
ft1 <- foldert(floribundity, rows.select = "union", same.rows = TRUE)
print(ft1)

# rows.select = "intersect", same.rows = TRUE
ft2 <- foldert(floribundity, rows.select = "intersect", same.rows = TRUE)
print(ft2)

# cols.select = "intersect"
ft3 <- foldert(floribundity, cols.select = "intersect")
print(ft3)

# cols.select = "union"
ft4 <- foldert(floribundity, cols.select = "union")
print(ft4)

# times is an ordered factor
ftimes <- ordered(names(floribundity))
ft5 <- foldert(floribundity, times = ftimes)
print(ft5)
print(attr(ft5, "times"))

# times is numeric
numtimes <- as.numeric(as.Date(names(floribundity)))
ft6 <- foldert(floribundity, times = numtimes)
print(ft6)
summary(attr(ft6, "times"))

# Choose the columns which are kept in each data frame
ft7 <- foldert(floribundity, cols.select = "rose")
print(ft7)
ft8 <- foldert(floribundity, cols.select = c("rose", "nflowers"))
print(ft8)
ft9 <- foldert(floribundity, cols.select = "nflowers")
print(ft9)
}
