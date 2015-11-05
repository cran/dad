\name{folderh2folder}
\alias{folderh2folder}
\title{
  Hierarchic folder of two data frames to folder of data frames whose column names are the same
}
\description{
  Creates an object of class \code{folder} from an object of class \code{folderh}.
}
\usage{
folderh2folder(foldh)
}
\arguments{
  \item{foldh}{
     object of class \code{\link{folderh}} with two data frames.
}
}
\value{
 \code{folderh2folder} returns an object of class \code{\link{folder}}.
}
\details{ 
 The number of data frames of the returned folder is the same as the number of rows of \code{foldh[[1]]}.
 
 For each \code{j}, the \code{j}-th data frame has two parts. The first part consists of the rows of \code{foldh[[2]]} corresponding to the \code{j}-th level of the key of \code{foldh[[1]]}. The second part consists of equal rows, that are the \code{j}-th row of \code{foldh[[1]]} (except the key).
}
\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard

Maintainer: Rachid Boumaza <rachid.boumaza@agrocampus-ouest.fr>
}

\seealso{
\code{\link{folder}}, \code{\link{folderh}}.
\code{\link{folder2df}} to build a data frame from an object of class \code{folder}.
\code{\link{folderh2df}} to build a data frame from an object of class \code{folderh}.
}
\examples{
# First example: leaves               
data(roseflowers)
flg <- roseflowers$variety
flx <- roseflowers$flower

flfh <- folderh(flg, flx, "numrose", "rose")
print(flfh)

flf <- folderh2folder(flfh)
print(flf)

# Second example: castles               
data(castles.dated)
cag <- castles.dated$periods
cax <- castles.dated$stones

cafh <- folderh(cag, cax, "castle", "castle")
print(cafh)

caf <- folderh2folder(cafh)
print(caf)
}
