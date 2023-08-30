\name{read.mtg}
\alias{read.mtg}
\alias{topology}
\title{
  Read a MTG (Multiscale Tree Graph) file
}
\description{
  Reads an MTG (Multiscale Tree Graph) file and returns an object of class \code{foldermtg}, that is a list of data frames (see Details).
}
\usage{
read.mtg(file, ...)
}
\arguments{
  \item{file}{
     character. Path of the MTG file.
}
  \item{...}{
      optional arguments to \code{print} methods.
}
}
\details{
 Recalling that a MTG file is a text file that can be opened with a spreadsheet (Excel, LibreOffice-Calc...).
 Its 4 tables are:
 \itemize{
 \item \bold{CLASSES:} In this table the first column, named \code{SYMBOL}, contains the symbolic character denoting each botanical entity (or vertex class, plant component...) used in the MTG (for example, P for plant, A for axis...). The second column, named \code{SCALE}, represents the scale at which each entity appears in the MTG (for example 1 for P, 2 for axis...).
  \item \bold{DESCRIPTION:} This table displays the relations between the vertices: \code{+} (branching relationship) or \code{<} (successor relationship).
 \item \bold{FEATURES:} This table contains the features that can be attached to the vertices and their types: \code{INT} (integer), \code{REAL} (real numbers), \code{STRING} (character)...
 \item \bold{MTG:} This table describes the plant topology, that is the vertices (one vertex per row) and their relations, the spatial coordinates of each vertex and the values taken by each vertex on the above listed features.
 
 Each vertex is labelled by its class, designating its botanical entity, and its index, designating its position among its immediate neighbours having the same scale. Each vertex label is preceded by \code{+} or \code{<}, seen above, or by the symbol \code{/} (decomposition relationship) that means that the corresponding vertex is the first vertex of the decomposition of the vertex which precedes \code{/}.

 Notice that the column number of a vertex matches with its branching order. The vertices of scale \code{k} resulting from the decomposition of a vertex of scale \code{k-1}, named parent vertex, have the same order as that of the parent vertex. 
}
 See the example below.
}
\value{
 \code{read.mtg} returns an object, say \code{x}, of class \code{fodermtg}, that is a list of at least 6 data frames:
 \item{classes }{the table \code{CLASSES:} in the MTG file.}
 \item{description }{the table \code{DESCRIPTION:} in the MTG file.}
 \item{features }{the table \code{FEATURES:} in the MTG file.}
 \item{topology }{data frame containing the first columns of the \code{"MTG:"} table of the MTG file. If the maximum branching order of the elements of the MTG is \eqn{p}, then \code{x$topology} has \eqn{p} columns.
 
 If the \code{i}-th vertex appears on the \code{j}-th column, it means that its branching order is \code{j}, that is it belongs to a vertex of the \code{j}-th order.}
 \item{coordinates }{data frame of the spatial coordinates of the entities. It has six columns: \code{XX}, \code{YY}, \code{ZZ} (cartesian coordinates), \code{AA}, \code{BB}, \code{CC} (angle coordinates). If there are no coordinates in the MTG file, this data frame has 0 row.}

The sixth and following elements are \code{nclass} data frames, \code{nclass} being the number of classes in the MTG file. Each data frame matches with a vertex class, such as \code{"P"} (plant), \code{"A"} (axes), \code{"M"} (metamers or phytomers), and contains the features on the corresponing vertices.
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
\code{\link{print.foldermtg}}

\code{\link{mtgorder}}
}

\references{
    Pradal, C., Godin, C. and Cokelaer, T. (2023). \href{https://mtg.readthedocs.io/en/latest/user/index.html}{MTG user guide}
}

\examples{
mtgfile1 <- system.file("extdata/plant1.mtg", package = "dad")
x1 <- read.mtg(mtgfile1)
print(x1)

mtgfile2 <- system.file("extdata/plant2.mtg", package = "dad")
x2 <- read.mtg(mtgfile2)
print(x2)
}
