\name{df2folder}
\alias{df2folder}
\title{
  Data frame to folder
}
\description{
  Builds an object of class \code{folder} from a data frame.
}
\usage{
df2folder(x, groups = tail(colnames(x), 1))
}
\arguments{
  \item{x}{
     data frame.
}
  \item{groups}{
     string. The name of the column of x containing the grouping variable. \code{x[, groups]} must be a factor, otherwise, there is an error.
     
     If omitted, the last column of \code{x} is used as grouping variable.
}
}
\value{
 \code{df2folder} returns an object of class \code{\link{folder}} that is a list of data frames which have the same column names.
 
 Each element of the folder contains the data corresponding to one level of \code{x[, groups]}.
}
\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard

Maintainer: Rachid Boumaza <rachid.boumaza@agrocampus-ouest.fr>
}

\seealso{
\code{\link{folder}}: objects of class \code{folder}.
\code{\link{folder2df}}: build a data frame from an object of class \code{folder}.
}
\examples{
# First example: iris (Fisher)               
data(iris)
iris.fold <- df2folder(iris, "Species")
print(iris.fold)

# Second example: roses
data(roses)
roses.fold <- df2folder(roses, "Lot")
print(roses.fold)
}
