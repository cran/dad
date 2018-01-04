\name{as.folderh.foldermtg}
\alias{as.folderh.foldermtg}
\title{
  Build a hierarchic folder from an object of class foldermtg
}
\description{
  Creates an object of class \code{\link{folderh}} from an object of class \code{\link{foldermtg}}.
}
\usage{
\method{as.folderh}{foldermtg}(x, classes)
}
\arguments{
  \item{x}{
     object of class \code{\link{foldermtg}}.
}
  \item{classes}{
     character vector. Codes of the vertex classes in the returned folderh.
     These codes are the names of the elements (data frames) of \code{x} containing the features on the vertices corresponding to the codes.
     
     These codes must be distinct, and the corresponding classes must have distinct scales (see \code{\link{foldermtg}}). Otherwise, there is an error.
     
     These codes, except the one with the highest scale, are the keys of the returned folderh.
}
}
\value{
 An object of class \code{folderh}. Its elements are the data frames of \code{x} containing the features on vertices. Hence, each data frame matches with a class of vertex, and a scale. These data frames are in increasing order of the scale.
 
 A column (factor) is added to the first data frame, containing the identifier of the vertex. 
 Two columns are added to the second data frame:
 \enumerate{
 \item the first one is a factor which gives, for each vertex, the name of the vertex of the first data frame which is its "parent",
 \item and the second one is also a factor and contains the vertex's identifier.
 }
 And so on for the third and following data frames, if relevant.
 
 The column containing the vertex identifiers is redundant with the row names; anyway, it is necessary for \code{\link{folderh}}.
 
 The key of the relationship between the two first data frame is given by the first column of each of these data frames.
 If there are more than two data frames, the key of the relationship between the \eqn{n}-th and \eqn{(n+1)}-th data frames (\eqn{n > 1}) is given by the second column of the \eqn{n}$th data frame and the first column of the \eqn{(n+1)}-th data frame.
}

\details{
 This function uses \code{\link{folderh}}.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{read.mtg}}: reads a MTG file and creates an object of class "foldermtg".
\code{\link{folderh}} : object of class \code{folderh}. 
}
\examples{
mtgfile <- system.file("extdata/plant1.mtg", package = "dad")
x <- read.mtg(mtgfile)

# folderh containing the plant ("P") and the stems ("A")
as.folderh(x, classes = c("P", "A"))

# folderh containing the plant ("P"), axes ("A") and phytomers ("M")
as.folderh(x, classes = c("P", "A", "M"))

# folderh containing the plant ("P") and the phytomers ("M")
as.folderh(x, classes = c("P", "M"))

# folderh containing the axes and phytomers
fhPM <- as.folderh(x, classes = c("A", "M"))
# coerce this folderh into a folder, and compute statistics on this folder
fPM <- as.folder(fhPM)
mean(fPM)
}
