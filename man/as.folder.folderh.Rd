\name{as.folder.folderh}
\alias{as.folder.folderh}
%\alias{folderh2folder}
%\alias{as.folder}
\title{
  Hierarchic folder to folder
}
\description{
  Creates an object of class \code{folder}, that is a list of data frames with the same column names, from a \code{folderh}.
}
\usage{
\method{as.folder}{folderh}(x, elt = names(x)[2], key = attr(x, "keys")[1], ...)
}
\arguments{
  \item{x}{
     object of class \code{\link{folderh}} containing N (N>1) data frames: \code{x[[1]]},..., \code{x[[N]]}, related by (N-1) keys: \code{keys[1]},..., \code{keys[N-1]}.
}
  \item{elt}{
     string. The name of one element of \code{x}, that is data frame, say the \code{j}-th, whose rows are distributed among the data frames of the returned folder. See details.
}
  \item{key}{
     string. The name of an element of \code{attr(x, "keys")}, that is the key, say the \code{k}-th  with \code{k}<\code{j}, which is the factor whose levels are the names of the data frames of the returned folder. See details.
}
  \item{...}{
     further arguments passed to or from other methods.
}
}
\value{
 \code{as.folder.folderh} returns an object of class \code{\link{folder}}, a list of data frames with the same columns. These data frames contain the values of \code{x[[elt]]} (or \code{x[[j]]}) and the corresponding values of the data frames \code{x[[j-1]]}, ... \code{x[[k]]}, these correspondances being defined by the keys of the hierarchic folder. The names of these data frames are given by the levels of the key \code{attr(x, "keys")[k])}.

 The rows of the data frame \code{x[[elt]]} (or \code{x[[j]]}) are distributed among the data frames of the returned folder accordingly to the levels of the key \code{attr(x, "keys")[k]}. So the row names of the \code{l}-th data frame of the returned folder consist in the rows of \code{x[[j]]} corresponding to the \code{l}-th level of the key \code{attr(x, "keys")[k]}. 
 
 The column names of the data frames of the returned folder are the union of the column names of the data frames \code{x[[k]]},..., \code{x[[j]]} and are organized in two parts. 
 \enumerate{
 \item The first part consists in the columns of \code{x[[k]]} except the column corresponding to the key \code{attr(x, "keys")[k]}. 
 \item For each \code{i=k+1,...,j} the column names of the data frame \code{x[[i]]} are reorganized so that the key \code{attr(x, "keys")[i]} is its first column. The columns of the reorganized data frames \code{x[[k+1]]},..., \code{x[[j]]} are concatenated. The result forms the second part. 
 } 

 Notice that if:
   \itemize{
   \item the folderh has two data frames \code{df1} and \code{df2}, where the factor corresponding to the key has \eqn{T} levels, and one column of \code{df2}, say \code{df2[, "Fa"]}, is a factor with levels \code{"a1"}, \dots, \code{"ap"}
   \item and the folder returned by \code{as.folder} includes \eqn{T} data frames \code{dat1}, \dots, \code{datT},
   }
 then each of \code{dat1}, \dots, \code{datT} has a column named \code{"Fa"} which is a factor \bold{with the same levels} \code{"a1"}, \dots, \code{"ap"} \bold{as} \code{df2[, "Fa"]}.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folder}}, \code{\link{folderh}}.
\code{\link{as.folder.folderh}} to build an object of class \code{folder} from an object of class \code{folderh}.
\code{\link{as.data.frame.folder}} to build a data frame from an object of class \code{folder}.
\code{\link{as.data.frame.folderh}} to build a data frame from an object of class \code{folderh}.
}
\examples{
# First example: flowers               
data(roseflowers)
flg <- roseflowers$variety
flx <- roseflowers$flower

flfh <- folderh(flg, "rose", flx)
print(flfh)

flf <- as.folder(flfh)
print(flf)

# Second example: castles               
data(castles.dated)
cag <- castles.dated$periods
cax <- castles.dated$stones

cafh <- folderh(cag, "castle", cax)
print(cafh)

caf <- as.folder(cafh)
print(caf)

# Third example: leaves (example of a folderh of more than two data frames)
data(roseleaves)
lvr <- roseleaves$rose
lvs <- roseleaves$stem
lvl <- roseleaves$leaf
lvll <- roseleaves$leaflet

lfh <- folderh(lvr, "rose", lvs, "stem", lvl, "leaf", lvll)

lf1 <- as.folder(lfh, elt = "lvs", key = "rose")
print(lf1)

lf2 <- as.folder(lfh, elt = "lvl", key = "rose")
print(lf2)

lf3 <- as.folder(lfh, elt = "lvll", key = "rose")
print(lf3)

lf4 <- as.folder(lfh, elt = "lvll", key = "stem")
print(lf4)
}
