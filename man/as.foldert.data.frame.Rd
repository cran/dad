\name{as.foldert.data.frame}
\alias{as.foldert.data.frame}
%\alias{as.foldert.default}
\title{
  Data frame to foldert
}
\description{
  Builds an object of class \code{\link{foldert}} from a data frame.
}
\usage{
\method{as.foldert}{data.frame}(x, method = 1, ind = 1, timecol = 2, nvar = NULL, same.rows = TRUE, ...)
}
\arguments{
  \item{x}{
     data frame.
}
  \item{method}{
     1 or 2. Indicates the layout of the data frame x and, therefore, the method used to extract the data and build the foldert.
     \itemize{
     \item{If \code{method = 1}, there is a column containing the identifiers of the measured objects and a column containing the times. The other columns contain the observations.}
     \item{If \code{method = 2}, there is a column containing the identifiers of the measured objects, and the observations are organized as follows:
      \itemize{
      \item{the observations corresponding to the 1st time are on columns \code{timecol : (timecol + nvar - 1)}}
      \item{the observations corresponding to the 2nd time are on columns \code{(timecol + nvar) : (timecol + 2 * nvar - 1)}}
      \item{and so on.}
      }
     }
     }
}
  \item{ind}{
     string or numeric. The name of the column of x containing the indentifiers of the measured objects, or the number of this column.
}
  \item{timecol}{
     string or numeric.
     \itemize{
     \item{If \code{method = 1}, \code{timecol} is the name or the number of the column of x containing the times of observation, or the number of this column. \code{x[, timecol]} must be of class \code{"numeric"}, \code{"ordered"}, \code{"Date"}, \code{"POSIXlt"} or \code{"POSIXct"}, otherwise, there is an error.
     }
     \item{If \code{method=2}, \code{timecol} is the name or the number of the first column corresponding to the first observation. If there are duplicated column names and several columns are named by \code{timecol}, the first one is considered.
     }
     }
}
  \item{nvar}{
     integer. If \code{method=2}, indicates the number of variables observed at each time.
     
     Omitted if \code{method=1}.
}
  \item{same.rows}{
     logical. If \code{TRUE} (default), the elements of the returned foldert are data frames with the same row names.
     
     Necessarily \code{TRUE} if \code{method = 2}.
}
  \item{...}{
     further arguments passed to or from other methods.
}
}
\value{
 an object \code{ft} of class \code{\link{foldert}}, that is a list of data frames organised according to time; these data frames have the same column names.
 
 If \code{method = 1}, they can have the same row names (\code{attr(ft, "same.rows") = TRUE}) or not (\code{attr(ft, "same.rows") = FALSE}).
 The time attribute \code{attr(ft, "times")} has the same class as \code{x[, timecol]} (numeric vector, ordered factor or object of class \code{"Date"}, \code{"POSIXlt"} or \code{"POSIXct"}) and contains the values of \code{x[, timecol]}.

 If \code{method = 2}, they necessarily have the same row names: \code{attr(ft, "same.rows") = TRUE}
 and \code{attr(ft, "times")} is \code{1:length(ft)}.

 The rownames of each data frame are the identifiers of the individuals, as given by \code{x[, ind]}.
 
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{foldert}}: objects of class \code{foldert}.

\code{\link{as.data.frame.foldert}}: build a data frame from an object of class \code{foldert}.

\code{\link{as.foldert.array}}: build an object of class \code{foldert} from a \eqn{3d}-array.
}
\examples{
# First example: method = 1

times <- as.Date(c("2017-03-01", "2017-04-01", "2017-05-01"))
x1 <- data.frame(t=times[1], ind=1:6,
                 f=c("a","a","a","b","b","b"), z1=rep(0,6), z2=rep(0,6),
                 stringsAsFactors = TRUE)
x2 <- data.frame(t=times[2], ind=c(1,4,6),
                 f=c("a","b","b"), z1=rnorm(3,1,1), z2=rnorm(3,3,2),
                 stringsAsFactors = TRUE)
x3 <- data.frame(t=times[3], ind=c(1,3:6),
                 f=c("a","a","a","b","b"), z1=rnorm(5,3,2), z2=rnorm(5,6,3),
                 stringsAsFactors = TRUE)
x <- rbind(x1, x2, x3)

ft1 <- as.foldert(x, method = 1, ind = "ind", timecol = "t", same.rows = TRUE)
print(ft1)

ft2 <- as.foldert(x, method = 1, ind = "ind", timecol = "t", same.rows = FALSE)
print(ft2)

data(castles.dated)
periods <- castles.dated$periods
stones <- castles.dated$stones
stones$stone <- rownames(stones)

castledf <- merge(periods, stones, by = "castle")
castledf$period <- as.numeric(castledf$period)
castledf$stone <- as.factor(paste(as.character(castledf$castle),
                            as.character(castledf$stone), sep = "_"))

castfoldt1 <- as.foldert(castledf, method = 1, ind = "stone", timecol = "period",
                         same.rows = FALSE)
summary(castfoldt1)


# Second example: method = 2

times <- as.Date(c("2017-03-01", "2017-04-01", "2017-05-01"))
y1 <- data.frame(z1=rep(0,6), z2=rep(0,6))
y2 <- data.frame(z1=rnorm(6,1,1), z2=rnorm(6,3,2))
y3 <- data.frame(z1=rnorm(6,3,2), z2=rnorm(6,6,3))
y <- cbind(ind = 1:6, y1, y2, y3)

ft3 <- as.foldert(y, method = 2, ind = "ind", timecol = 2, nvar = 2)
print(ft3)
}