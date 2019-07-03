\name{interpret.mdsdd}
\alias{interpret.mdsdd}

\title{
    Scores of the \code{mdsdd} function vs. marginal probability distributions or association measures
}
\description{
    Applies to an object of class \code{"mdsdd"}, plots the scores vs. the marginal probability distributions or pairwise association measures of the discrete variables, and computes the correlations between these scores and probabilities or association measures (see Details).
}
\usage{
\method{interpret}{mdsdd}(x, nscore = 1:3, mma = c("marg1", "marg2", "assoc"), ...)
}
\arguments{
  \item{x}{
       object of class \code{mdsdd} (returned by the \code{\link{mdsdd}} function).
}
  \item{nscore}{
       numeric vector. Selects the columns of the data frame \code{x$scores} consisting of the score vectors.
       
       Warning: Its components cannot be greater than the \code{nb.factors} argument in the call of the \code{\link{mdsdd}} function.
}
  \item{mma}{
       character. Indicates which measures will be considered:
       \itemize{
          \item{"marg1": the probability distribution of each variable.}
          \item{"marg2": the joint probability distribution of each pair of variables.}
          \item{"assoc": the pairwise association measures of the variables.}
       }
}
  \item{...}{
       Arguments to be passed to methods.
}
}
\details{
    A new graphics device is opened for each score column. A device can contain up to 9 graphs.
    If there are too many (more than 36) graphs for each score, a multipage PDF file is created in the current working directory, and the graphs are displayed in it.

    The number of principal scores to be interpreted cannot be greater than \code{nb.factors} of the data frame \code{x$scores} returned by the function \code{\link{mdsdd}}.

}
\value{
    Returns a list including:
    \item{pearson }{matrix of Pearson correlations between selected scores and probabilities or association measures.}
    \item{spearman }{matrix of Spearman correlations between selected scores and probabilities or association measures.}
}

\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}

\seealso{
   \link{mdsdd}; \link{plot.mdsdd}.
}
\examples{
# INSEE (France): Diploma x Socio professional group, seven years.
data(dspg)
xlista = dspg
a <- mdsdd(xlista)
interpret(a)

# Example 3 with a list of 96 arrays (departments)
data(dspgd2015)
xd = dspgd2015
res = mdsdd(xd, group.name = "coded")
interpret(res)
plot(res, fontsize.points = 0.7)

# Each department is represented by its name
data(departments)
coor = merge(res$scores, departments, by = "coded")
dev.new()
plot(coor$PC.1, coor$PC.2, type ="n")
text(coor$PC.1, coor$PC.2, coor$named, cex = 0.5)

# Each department is represented by its region
dev.new()
plot(coor$PC.1, coor$PC.2, type ="n")
text(coor$PC.1, coor$PC.2, coor$coder, cex = 0.7)
}
