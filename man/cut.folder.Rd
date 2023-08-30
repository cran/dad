\name{cut.folder}
\alias{cut.folder}
\title{
  In a folder: change numeric variables into factors
}
\description{
  This function applies to a \link{folder}. For each elements (data frames) of this folder, it changes its numerical columns into factors, using \code{\link{cut.data.frame}}.
}
\usage{
\method{cut}{folder}(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, dig.lab = 3L,
    ordered_result = FALSE, cutcol = NULL, ...)
}
\arguments{
  \item{x}{
       an object of class \code{\link{folder}}.
}       
  \item{breaks}{
       list or numeric, defining the intervals into which the variables of each element of the folder is to be cut.
       See \code{\link{cut.folder}}.
}
  \item{labels}{
       list of character vectors. If not omitted, it gives the labels for the intervals of each column of the elements of \code{x}.
       See \code{\link{cut.folder}}.
}
  \item{include.lowest}{
       logical, indicating if a value equal to the lowest (or highest, for \code{right = FALSE}) 'breaks' value should be included (see \code{\link{cut.folder}}).
}
  \item{right}{
       logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa (see \code{\link{cut.folder}}).
}
  \item{dig.lab}{
       integer or integer vector, which is used when labels are not given.
       It determines the number of digits used in formatting the break numbers.
       See \code{\link{cut.folder}}.
}
  \item{ordered_result}{
       logical: should the results be ordered factors? (see \code{\link{cut.folder}})
}
  \item{cutcol}{
       numeric vector: indices of the columns of the elements of \code{x} to be converted into factors.
       These columns must all be numeric. Otherwise, there is a warning.
       See \code{\link{cut.folder}}.
}
  \item{...}{
       further arguments passed to or from other methods.
}
}
%\details{
%
%}
\value{
    An object of class \code{folder} with the same length and names as \code{x}.
    Its elements (data frames) have the same column and row names as the elements of \code{x}.
    
    For more details, see \code{\link{cut.data.frame}}
}
%\references{
%
%}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}
\examples{
data("roses")

x <- as.folder(roses[, c("Sha", "Den", "Sym", "rose")], groups = "rose")
summary(x)

x3 <- cut(x, breaks = 3)
summary(x3)

x7 <- cut(x, breaks = 7)
summary(x7)
}
