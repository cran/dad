\name{association measures for folder}
\alias{cramer.folder}
\alias{tschuprow.folder}
\alias{pearson.folder}
\alias{phi.folder}
\title{
  Association measures between categorical variables of the data frames of a folder
}
\description{
  Computes the pairwise association measures (Cramer's V, Pearson's contingency coefficient, phi, Tschuprow's T) between the categorical variables of an object of class \code{folder}. The computation is carried out using the functions \code{\link{cramer.data.frame}}, \code{\link{tschuprow.data.frame}}, \code{\link{pearson.data.frame}} or \code{\link{phi.data.frame}}. These functions are built from corresponding functions of the package \code{DescTools} (see \code{\link{Assocs}})
}
\usage{
cramer.folder(xf)
tschuprow.folder(xf)
pearson.folder(xf)
phi.folder(xf)
}
\arguments{
  \item{xf}{
     an object of class \code{\link{folder}} that is a list of data frames with the same column names. Its columns should be factors, otherwise there is a warning.
}
}
\value{
 A list the length of which is equal to the number of data frames of the folder. Each element of the list is a square matrice giving the pairwise association measures of the variables of the corresponding data frame.
}
\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}
\examples{
data(roses)
xr = roses[,c("Sha", "Den", "Sym", "rose")]
xr$Sha = cut(xr$Sha, breaks = c(0, 5, 7, 10))
xr$Den = cut(xr$Den, breaks = c(0, 4, 6, 10))
xr$Sym = cut(xr$Sym, breaks = c(0, 6, 8, 10))
xfolder = as.folder(xr, groups = "rose")
cramer.folder(xfolder)
pearson.folder(xfolder)
phi.folder(xfolder)
tschuprow.folder(xfolder)
}