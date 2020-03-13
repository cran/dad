\name{cut.data.frame}
\alias{cut.data.frame}
\title{
  Change numeric variables into factors
}
\description{
  This function changes numerical columns of a data frame \code{x} into factors. For each of these columns, its range is divided into intervals and the values of this column is recoded according to which interval they fall.
  
  For that, \code{\link{cut}} is applied to each column of \code{x}.
}
\usage{
\method{cut}{data.frame}(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, dig.lab = 3L,
    ordered_result = FALSE, cutcol = NULL, ...)
}
\arguments{
  \item{x}{
       data frame (can also be a tibble).
}       
  \item{breaks}{
       list or numeric.
       \itemize{
       \item If breaks is a list, its length is equal to the number of columns in the data frame. It can be:
          \itemize{
          \item a list of numeric vectors.
          The \eqn{j^{th}} element corresponds to the column \code{x[, j]}, and is a vector of two or more unique cut points
          \item or a list of single numbers (each greater or equal to 2).
          \code{breaks[[j]]} element gives the number of intervals into which th \eqn{j^{th}} variable of the folder is to be cut.
          The elements \code{breaks[[j]]} corresponding to non-numeric columns must be NULL; if not, there is a warning.
          }
       \item If breaks is a numeric vector, it gives the number of intervals into which every column \code{x[, j]} is to be cut (see \code{\link{cut}}).
       }
}
  \item{labels}{
       list of character vectors. If given, its length is equal to the number of columns of x.
       \code{labels[[j]]} gives the labels for the intervals of the \eqn{j^{th}} columns of the data frame.
       By default, the \code{labels} are constructed using "(a,b]" interval notation.
       If \code{labels = FALSE}, simple integer codes are returned instead of a factor.
       
       See \code{\link{cut}}.
}
  \item{include.lowest}{
       logical, indicating if, for each column \code{x[, j]}, an \code{x[i, j]} equal to the lowest (or highest, for \code{right = FALSE}) 'breaks' value should be included (see \code{\link{cut}}).
}
  \item{right}{
       logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa (see \code{\link{cut}}).
}
  \item{dig.lab}{
       integer or integer vector, which is used when labels are not given.
       It determines the number of digits used in formatting the break numbers.
       \itemize{
       \item If it is a single value, it gives the number of digits for all variables of the folder (see \code{\link{cut}}).
       \item If it is a list of integers, its length is equal to the number of variables, and the \eqn{j^{th}} element gives the number of digits for the \eqn{j^{th}} variable of the folder.
       }
}
  \item{ordered_result}{
       logical: should the results be ordered factors? (see \code{\link{cut}})
}
  \item{cutcol}{
       numeric vector: indices of the columns to be converted into factors.
       These columns must all be numeric. Otherwise, there is a warning.
}
  \item{...}{
       further arguments passed to or from other methods.
}
}
%\details{
%
%}
\value{
    A data frame with the same column and row names as \code{x}.
    
    If \code{cutcol} is given, each numeric column \code{x[, j]} whose number is contained in \code{cutcol} is replaced by a factor.
    The other columns are unmodified.
    
    If any column \code{x[, j]} whose number is in \code{cutcol} is not numeric, it is unmodified.
    
    If \code{cutcol} is omitted, every numerical columns are replaced by factors.
}
%\references{
%
%}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}
\examples{
data("roses")
x <- roses[roses$rose \%in\% c("A", "B"), c("Sha", "Sym", "Den", "rose")]

cut(x, breaks = 3)
cut(x, breaks = 5)
cut(x, breaks = c(0, 4, 6, 10))
cut(x, breaks = list(c(0, 6, 8, 10), c(0, 5, 7, 10), c(0, 6, 7, 10)))
cut(x, breaks = list(c(0, 6, 8, 10), c(0, 5, 7, 10)), cutcol = 1:2)
}
