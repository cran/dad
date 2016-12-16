\name{folderh}
\alias{folderh}
\title{
  Hierarchic folder of n data frames related in pairs by (n-1) keys
}
\description{
  Creates an object of class \code{folderh}, that is a list of \eqn{n>1} data frames whose rows are related by (n-1) keys, each key defining a relation "1 to N" between the two adjacent data frames passed as arguments of the function.
}
\usage{
folderh(df1, key1, df2, ..., na.rm = TRUE)
}
\arguments{
  \item{df1}{
     data frame with at least two columns. It contains a factor (whose name is given by \code{key1} argument) whose levels are taken exactly once.
}
  \item{key1}{
     character string. The name of the factor of the data frames \code{df1} and \code{df2} which contains the key of the relations "1 to N" between the two datasets.
}
  \item{df2}{
     data frame with at least two columns. It contains a factor column (named by \code{keys} argument) with the same levels as \code{df1[, key1]} (see Details).
}
  \item{...}{
     optional. One or several supplementary character strings and data frames, ordered as follows: \code{key2, df3, ...}.
     The argument \code{key2} indicates the key defining the relation "1 to N" between the data frames \code{df2} and \code{df3}, and so on.
}
  \item{na.rm}{
     logical. If \code{TRUE}, the rows of each data frame for which the key is \code{NA} are removed.
}
}
\value{
 Returns an object of class \code{folderh}. Its elements are the data frames passed as arguments, and the attribute \code{attr(, "keys")} contains the character arguments.
}

\details{
The object of class \code{folderh} is a list of \eqn{n \ge 2} data frames.
\itemize{
\item If no optional arguments are given via \code{...}, that is \eqn{n = 2}, the two data frames of the list
have a column named by the attribute \code{attr(, "keys")} (argument \code{key1}), which is a factor with the same levels.
Each one of these levels occur exactly once in the first data frame of the list.
\item If some supplementary data frames and supplementary strings \code{key2, df3}, ... are given as optional arguments, \eqn{n} is the number of data frames given as arguments.
Then, the attribute \code{attr(, "keys")} is a vector of \eqn{n-1} character strings.
For \eqn{i = 1, \ldots, N-1}, its \eqn{i}-th element is the name of a column of the \eqn{i}-th and \eqn{(i+1)}-th data frames of the folderh, which are factors with the same levels.
Each one of these levels occur exactly once in the \eqn{i}-th data frame.
}

If there are more than two data frames, \code{folderh} computes a folderh with the two last data frames, and then uses the function \code{\link{appendtofolderh}} to append each one of the other data frames to the folderh.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{is.folderh}} to test if an object is of class \code{folder}.
\code{\link{folder}} for a folder of data frames with no hierarchic relation between them.
\code{\link{as.folder.folderh}} to build an object of class \code{folder} from an object of class \code{folderh}, 
\code{\link{as.data.frame.folderh}} to build a data frame.
}
\examples{
# First example: rose flowers
data(roseflowers)
df1 <- roseflowers$variety
df2 <- roseflowers$flower
fh1 <- folderh(df1, "rose", df2)
print(fh1)

# Second example
data(roseleaves)
roses <- roseleaves$rose
stems <- roseleaves$stem
leaves <- roseleaves$leaf
leaflets <- roseleaves$leaflet
fh2 <- folderh(roses, "rose", stems, "stem", leaves, "leaf", leaflets)
print(fh2)
}
