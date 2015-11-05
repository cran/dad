\name{folderh}
\alias{folderh}
\title{
  Folder of two data sets related by a hierarchy
}
\description{
  Creates an object of class \code{folderh}, that is a list of 2 data frames whose rows are related by two keys defining a relation "1 to N".
}
\usage{
folderh(g, x, keyg = "rownames", keyx, nag.rm = TRUE, nax.rm = TRUE)
}
\arguments{
  \item{g}{
     data frame with at least one column if \code{keyg} is \code{"rownames"}, or two columns if \code{keyg} is not \code{"rownames"}.
     
     If \code{keyg} is not \code{"rownames"}, the data frame contains a factor (named \code{keyg}) whose levels are taken once.
}
  \item{x}{
     data frame with at least two columns.
     
     It contains a factor column (named \code{keyx}) whose levels are the rownames of \code{g} (if \code{keyg = rownames}) or the levels of \code{keyg} (if \code{keyg} is a factor of the data frame \code{g}).
}
  \item{keyg}{
     string. It is either \code{"rownames"} (default) or the name of the factor of \code{g} which contains the key of the relation "1 to N".
}
  \item{keyx}{
     string. It is the name of the factor of \code{x} which contains the key of the relation "1 to N".
}
  \item{nag.rm}{
     logical. If \code{TRUE}, the rows of \code{g} for which \code{keyg} is \code{NA} are removed.
}
  \item{nax.rm}{
     logical. If \code{TRUE}, the rows of \code{x} for which \code{keyx} is \code{NA} are removed.
}
}
\value{
 Returns an object of class \code{folderh} that is a list of the two data frames \code{g} and \code{x}.
 
 This object has one attribute \code{attr(,"keys")} which is a character vector of two elements \code{c(key1, key2)}: \code{key1} is the column name of the key in \code{g}, and \code{key2} is the column name of the key in \code{x}.
}

\details{
If the argument \code{keyg} is \code{"rownames"} (the default), a column named \code{key} is added to the data frame \code{g}.
}

\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard

Maintainer: Rachid Boumaza <rachid.boumaza@agrocampus-ouest.fr>
}

\seealso{
\code{\link{is.folderh}} to test if an object is of class \code{folder}.
\code{\link{folder}} for a folder of data frames with no hierarchic relation between them.
\code{\link{folderh2folder}} to build an object of class \code{folder} from an object of class \code{folderh}, 
\code{\link{folderh2df}} to build a data frame.
}
\examples{
# First example: rose flowers               
data(roseflowers)
flg <- roseflowers$variety
flx <- roseflowers$flower

fh1 <- folderh(flg, flx, "rownames", "rose")
print(fh1)

fh2 <- folderh(flg, flx, "numrose", "rose")
print(fh2)

# Second example: castles               
data(castles.dated)
cag <- castles.dated$periods
cax <- castles.dated$stones

cafh <- folderh(cag, cax, "castle", "castle")
print(cafh)
}
