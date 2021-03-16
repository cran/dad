\name{hclustdd}
\alias{hclustdd}

\title{
Hierarchic cluster analysis of discrete probability distributions
}
\description{
Performs functional hierarchic cluster analysis of discrete probability distributions. It returns an object of class \code{\link{hclustdd}}. It applies \code{\link{hclust}} to the distance matrix between the \eqn{T} distributions.
}
\usage{
hclustdd(xf, group.name = "group", distance = c("l1", "l2", "chisqsym", "hellinger",
             "jeffreys", "jensen", "lp"), %association = c("cramer", "tschuprow", "pearson",  "phi"),
             sub.title = "", filename = NULL,
             method.hclust = "complete")
}
\arguments{
  \item{xf}{
       object of class \code{\link{folder}}, or list of arrays (or tables).
       
       \itemize{
       \item If it is a folder, its elements are data frames with \eqn{q} columns (considered as factors).
       The \eqn{t^{th}} element (\eqn{t = 1, \ldots, T}) matches with the \eqn{t^{th}} group.
       

       \item If it is a data frame, the columns with name given by the \code{group.name} argument is a factor giving the groups.
       The other columns are all considered as factors.
       
       \item If it is a list of arrays (or tables), the \eqn{t^{th}} element (\eqn{t = 1, \ldots, T}) is the table of the joint frequency distribution of \eqn{q} variables within the \eqn{t^{th}} group. The frequency distribution is expressed with relative or absolute frequencies.
       These arrays have the same shape.
       
       Each array (or table) \code{xf[[i]]} has:
          \itemize{
          \item the same dimension(s). If \eqn{q = 1} (univariate), \code{dim(xf[[i]])} is an integer. If \eqn{q > 1} (multivariate), \code{dim(xf[[i]])} is an integer vector of length \code{q}.
          \item the same dimension names \code{dimnames(xf[[i]])} (is non \code{NULL}). These dimnames are the names of the variables.
          }
       
       The elements of the arrays are non-negative numbers (if they are not, there is an error).
       }

}       
  \item{group.name}{
       string. Name of the grouping variable. Default: \code{group.name  = "group"}.
}
  \item{distance}{
       The distance or divergence used to compute the distance matrix between the discrete distributions (see Details).
       It can be:
       \itemize{
       \item \code{"l1"} (default) the \eqn{L^p} distance with \eqn{p = 1}
       \item \code{"l2"} the \eqn{L^p} distance with \eqn{p = 2}
       \item \code{"chisqsym"} the symmetric Chi-squared distance
       \item \code{"hellinger"} the Hellinger metric (Matusita distance)
       \item \code{"jeffreys"} Jeffreys distance (symmetrised Kullback-Leibler divergence)
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
xr = cut(xr, breaks = list(c(0, 5, 7, 10), c(0, 4, 6, 10), c(0, 6, 8, 10)))
xf = as.folder(xr, groups = "rose")
af = hclustdd(xf)
print(af)
print(af, dist.print = TRUE)
plot(af)
plot(af, hang = -1)

# Example 2 with a data frame obtained by converting numeric variables
ar = hclustdd(xr, group.name = "rose")
print(ar)
print(ar, dist.print = TRUE)
plot(ar)
plot(ar, hang = -1)

# Example 3 with a list of 7 arrays
data(dspg)
xl = dspg
hclustdd(xl)
}
