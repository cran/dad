\name{appendtofolderh}
\alias{appendtofolderh}
\alias{append.df2folderh}
\title{
  Adds a data frame to a \code{folderh}.
}
\description{
  Creates an object of class \code{folderh} by appending a data frame to an object of class \code{folderh}.
  The appended data frame will be the first or last element of the returned \code{folderh}.
}
\usage{
appendtofolderh(fh, df, key, after = FALSE)
}
\arguments{
  \item{fh}{
     object of class \code{\link{folderh}}.
}
  \item{df}{
     data frame to be appended to \code{fh}.
}
  \item{key}{
     character string. The key defining the relation \eqn{1 to N} between \code{df} and the first (if after = FALSE, the default value) or last (if after = TRUE) data frame of \code{fh}.
}
  \item{after}{
     logical. If \code{FALSE} (default), the data frame \code{df} is related to the first data frame of \code{fh}, and is appended as the first element of the returned folderh.
     If \code{TRUE}, \code{df} is related to the last data frame of \code{fh} and becomes the last element of the returned folderh.
}
}
\value{
 Returns an object of class \code{\link{folderh}}, that is a list of \eqn{n+1} data frames where \eqn{n} is the number of data frames of \code{fh}.
 The value of the attribute \code{attr(, "keys")} is \code{c(key, attr(fh, "keys"))} if \code{after = FALSE}), \code{c(attr(fh, "keys"), key)} otherwise.
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folderh}}.
}
