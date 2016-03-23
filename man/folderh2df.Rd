\name{folderh2df}
\alias{folderh2df}
\title{
  Hierarchic folder of two data frames to data frame
}
\description{
  Builds a data frame from a \code{folderh} of two data frames.
}              
\usage{
folderh2df(foldh)
}
\arguments{
  \item{foldh}{
     object of class \code{\link{folderh}} containing two data frames.
}
}
\value{
 \code{folderh2df} returns a data frame whose column names are the union of the column names of the data frame \code{foldh[[1]]} (except \code{keys[1]}) and that of the data frame \code{foldh[[2]]}.
}
\details{
 The number of rows of the returned data frame is equal to that of the data frame \code{foldh[[2]]}. The columns are divided into three parts. The first part consists of a single column which is the key \code{keys[2]} of the data frame \code{foldh[[2]]}. The second part consists of the columns of the data frame \code{foldh[[2]]} (except \code{keys[2]}). The third part consists of the columns of the data frame \code{foldh[[1]]} (except \code{keys[1]}) and its \code{i}-th row is the row of \code{foldh[[1]]} corresponding to the level \code{foldh[[2]][i,keys[2]]} (See the first example to view these details). 
}

\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard

Maintainer: Rachid Boumaza <rachid.boumaza@agrocampus-ouest.fr>
}

\seealso{
\code{\link{folder}}, \code{\link{folderh}}, \code{\link{folderh2folder}}. 
}
\examples{
# First example: rose flowers               
data(roseflowers)
flg <- roseflowers$variety
flx <- roseflowers$flower

flfh <- folderh(flg, "rose", flx)
print(flfh)

fldf <- folderh2df(flfh)
print(fldf)

# Second example: castles               
data(castles.dated)
cag <- castles.dated$periods
cax <- castles.dated$stones

cafh <- folderh(cag, "castle", cax)
print(cafh)

cadf <- folderh2df(cafh)
print(summary(cadf))
}                                                      
