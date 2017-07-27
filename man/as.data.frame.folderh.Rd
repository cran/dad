\name{as.data.frame.folderh}
\alias{as.data.frame.folderh}
\alias{folderh2df}
%\alias{as.data.frame}
\title{
  Hierarchic folder to data frame
}
\description{
  Builds a data frame from a \code{folderh}.
}              
\usage{
\method{as.data.frame}{folderh}(x, row.names = NULL, optional = FALSE, ...,
        elt = names(x)[2], key = attr(x, "keys")[1])
}
\arguments{
  \item{x}{
     object of class \code{\link{folderh}} containing N (N>1) data frames: \code{x[[1]]},..., \code{x[[N]]}, related by \code{(N-1)} keys: \code{keys[1]},..., \code{keys[N-1]}.
 }
  \item{row.names, optional}{
     for consistency with \code{\link{as.data.frame}}. Not taken into account.
 }
  \item{...}{
     further arguments passed to or from other methods.
 }
  \item{elt}{
     string. The name of one element of \code{x}, that is the data frame, say the \code{j}-th, whose rows are the rows of the returned data frame. See details.
 }
  \item{key}{
     string. The name of an element of \code{attr(x, "keys")}, that is the key, say the \code{k}-th with \code{k}<\code{j}, which is the factor designating the last column of the returned data frame. See details.
 }
}
\value{
 \code{as.data.frame.folderh} returns a data frame whose row names are those of \code{x[[elt]]} (that is \code{x[[j]]}). The data frame contains the values of \code{x[[elt]]} and the corresponding values of the data frames \code{x[[k]]}, these correspondances being defined by the keys of the hierarchic folder.

 The column names of the returned data frame are organized in three parts. 
 \enumerate{
 \item The first part consists in the key names \code{keys[k]},..., \code{keys[j-1]}.  
 \item The second part consists in the values of  \code{x[[j]]}.
 \item The third part consists in the values of \code{x[[k]]} except the key \code{keys[k]}.
 }
 See the examples to view these details.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{folder}}, \code{\link{folderh}}, \code{\link{as.folder.folderh}}. 
}
\examples{
# First example: rose flowers               
data(roseflowers)
flg <- roseflowers$variety
flx <- roseflowers$flower

flfh <- folderh(flg, "rose", flx)
print(flfh)

fldf <- as.data.frame(flfh)
print(fldf)

# Second example: castles               
data(castles.dated)
cag <- castles.dated$periods
cax <- castles.dated$stones

cafh <- folderh(cag, "castle", cax)
print(cafh)

cadf <- as.data.frame(cafh)
print(summary(cadf))

# Third example: leaves (example of a folderh with more than two data frames)
data(roseleaves)
lvr <- roseleaves$rose
lvs <- roseleaves$stem
lvl <- roseleaves$leaf
lvll <- roseleaves$leaflet

lfh <- folderh(lvr, "rose", lvs, "stem", lvl, "leaf", lvll)

lf1 <- as.data.frame(lfh, elt = "lvs", key = "rose")
print(lf1)

lf2 <- as.data.frame(lfh, elt = "lvl", key = "rose")
print(lf2)

lf3 <- as.data.frame(lfh, elt = "lvll", key = "rose")
print(lf3)

lf4 <- as.data.frame(lfh, elt = "lvll", key = "stem")
print(lf4)
}                                                      
