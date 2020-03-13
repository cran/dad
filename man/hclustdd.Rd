\name{hclustdd}
\alias{hclustdd}

\title{
Hierarchic cluster analysis of discrete probability distributions
}
\description{
Performs functional hierarchic cluster analysis of discrete probability distributions. It returns an object of class \code{\link{hclustdd}}. It applies \code{\link{hclust}} to the distance matrix between the \eqn{T} distributions.
}
\usage{
hclustdd(xf, distance = c("l1", "l2", "chisqsym", "hellinger",
             "jeffreys", "jensen", "lp"),
             %association = c("cramer", "tschuprow", "pearson",  "phi"),
             sub.title = "", filename = NULL,
             method.hclust = "complete", group.name = "group")
}
\arguments{
  \item{xf}{
       object of class \code{\link{folder}}, or list of arrays (or tables).
       
       If it is a folder, its elements are data frames with \eqn{q} numeric columns. The \eqn{t^{th}} element (\eqn{t = 1, ... , T}) matches with the \eqn{t^{th}} group.
       
       If it is a list of arrays (or tables), the \eqn{t^{th}} element (\eqn{t = 1, \ldots, T}) is the table of the joint distribution 
       (absolute or relative frequencies) of the \eqn{t^{th}} group. These arrays have the same shape, and their elements are non-negative             numbers (if they are not, there is an error).
}       
  \item{distance}{
       The distance or divergence used to compute the distance matrix between the discrete distributions (see Details).
       It can be:
       \itemize{
       \item \code{"l1"} (default) the \eqn{L^p} distance with \eqn{p = 1}
       \item \code{"l2"} the \eqn{L^p} distance with \eqn{p = 2}
       \item \code{"chisqsym"} the symmetric Chi-squared distance
       \item \code{"hellinger"} the Hellinger metric (Matusita distance)
       \item \code{"jeffreys"} the Jeffreys distance (symmetrised Kullback-Leibler divergence)
       \item \code{"jensen"} the Jensen-Shannon distance
       \item \code{"lp"} the \eqn{L^p} distance with \eqn{p} given by the argument \code{p} of the function.  
       }
}
%  \item{association}{
%       The association measure between two discrete distributions to be used (see Details).
%       It can be:
%       \itemize{
%       \item \code{"cramer"} (default) Cramer's V (see \code{\link{cramer.folder}}).
%       \item \code{"tschuprow"} Tschuprow's T (\code{\link{tschuprow.folder}}).
%       \item \code{"pearson"} Pearson's contingency coefficient (\code{\link{pearson.folder}}).
%       \item \code{"phi"} phi (\code{\link{phi.folder}}).
%       }
%}
  \item{sub.title}{
       string. If provided, the subtitle for the graphs.
}
  \item{filename}{
       string. Name of the file in which the results are saved. By default (\code{filename  = NULL}) the results are not saved.
}
  \item{method.hclust}{
       the agglomeration method to be used for the clustering. See the \code{method} argument of the \code{\link{hclust}} function.
}
%  \item{members}{
%       \code{NULL} or a vector with length the number of groups, i.e. the length of \code{xf}. See \code{\link{hclust}}.
%       \code{NULL} or a vector with length size of d. See \code{\link{hclust}}.
%}
  \item{group.name}{
       string. Name of the grouping variable. Default: \code{group.name  = "group"}.
}
}
\details{ 
    In order to compute the distances/dissimilarities between the groups, the \eqn{T} probability distributions \eqn{f_t} corresponding to the \eqn{T} groups of individuals are estimated from observations.
    Then the distances/dissimilarities between the estimated distributions are computed, using the distance or divergence defined by the \code{distance} argument:

    If the distance is \code{"l1"}, \code{"l2"} or \code{"lp"}, the distances are computed by the function \code{\link{matddlppar}}.
    Otherwise, it can be computed by \code{\link{matddchisqsympar}} (\code{"chisqsym"}), \code{\link{matddhellingerpar}} (\code{"hellinger"}), \code{\link{matddjeffreyspar}} (\code{"jeffreys"}) or \code{\link{matddjensenpar}} (\code{"jensen"}).
%
%    The association measures are computed accordingly to the value of the parameter \code{association}The computation uses the corresponding function of the package \code{DescTools} (see \code{\link{Assocs}}). Notice that an association measure between a constant variable with and other variable is set to zero. The association measure between each variable with itself is not computed and the diagonal of the returned association matrices is set to \code{NA}.
}
\value{
    Returns an object of class \code{hclustdd}, that is a list including:
    \item{distances }{matrix of the \eqn{L^2}-distances between the estimated densities.}
    \item{clust }{an object of class \code{\link{hclust}}.}
}
%\references{
%    
%}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}

\seealso{
    \link{hclustdd}
}
\examples{
# Example 1 with a folder (10 groups) of 3 factors 
# obtained by converting numeric variables 
data(roses)
xr = roses[,c("Sha", "Den", "Sym", "rose")]
xr$Sha = cut(xr$Sha, breaks = c(0, 5, 7, 10))
xr$Den = cut(xr$Den, breaks = c(0, 4, 6, 10))
xr$Sym = cut(xr$Sym, breaks = c(0, 6, 8, 10))
xf = as.folder(xr, groups = "rose")
a = hclustdd(xf)
print(a)
print(a, dist.print = TRUE)
plot(a)
plot(a, hang = -1)

# Example 2 with a list of 7 arrays
data(dspg)
xl = dspg
hclustdd(xl)
}
