\name{folder2df}
\alias{folder2df}
\title{
  Folder to data frame
}
\description{
  Builds a data frame from an object of class \code{folder}.
}
\usage{
folder2df(fold)
}
\arguments{
  \item{fold}{
     object of class \code{\link{folder}} that is a list of data frames. These data frames have the same column names.
}
}

\value{
 \code{folder2df} returns a data frame.
}

\details{The data frame is simply obtained by row binding the data frames of the folder and adding a factor (as last column), named \code{groups}. The levels of this factor are the names of the elements of the folder.
}

\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard

Maintainer: Rachid Boumaza <rachid.boumaza@agrocampus-ouest.fr>
}

\seealso{
\code{\link{folder}}: object of class \code{folder}.
\code{\link{df2folder}}: build an object of class \code{folder} from a data frame.
}
\examples{
data(iris)

iris.fold <- df2folder(iris, "Species")
print(iris.fold)

iris.df <- folder2df(iris.fold)
print(iris.df)
}                                                      
