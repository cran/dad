\name{mdsdd}
\alias{mdsdd}

\title{
Multidimensional scaling of discrete probability distributions
}
\description{
Applies the multidimensional scaling (MDS) method to discrete probability distributions in order to describe \eqn{T} groups of individuals on which are observed \eqn{q} categorical variables. It returns an object of class 
\code{mdsdd}. It applies \code{\link{cmdscale}} to the distance matrix between the \eqn{T} distributions.
}
\usage{
mdsdd(xf, group.name = "group", distance = c("l1", "l2", "chisqsym", "hellinger",
    "jeffreys", "jensen", "lp"), nb.factors = 3, nb.values = 10, association = c("cramer",
    "tschuprow", "pearson", "phi"), sub.title = "", plot.eigen = TRUE,
    plot.score = FALSE, nscore = 1:3, filename = NULL, add = TRUE, p)
}
\arguments{
  \item{xf}{
       object of class \code{\link{folder}}, list of arrays (or tables) or data frame.
       
       \itemize{
       \item If it is a folder, its elements are data frames with \eqn{q} columns (considered as factors).
       The \eqn{t^{th}} element (\eqn{t = 1, \ldots, T}) matches with the \eqn{t^{th}} group.
       
%       If it is a list of arrays (or tables), the \eqn{t^{th}} element (\eqn{t = 1, \ldots, T}) is the table of the joint distribution 
%       (absolute or relative frequencies) of the \eqn{t^{th}} group. These arrays have the same shape, and their elements are non-negative numbers (if they are not, there is an error).
       
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
       string. Name of the grouping variable. Default: \code{groupname  = "group"}.
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
  \item{nb.factors}{
       numeric. Number of returned principal coordinates (default \code{nb.factors = 3}). This number must be less than \eqn{T - 1}.
       
       Warning: The \code{\link{plot.mdsdd}} and \code{\link{interpret.mdsdd}} functions cannot take into account more than \code{nb.factors}  principal factors.
}
  \item{nb.values}{
       numeric. Number of returned eigenvalues (default \code{nb.values = 10}).
}
  \item{association}{
       The association measure between two discrete distributions to be used (see Details).
       It can be:
       \itemize{
       \item \code{"cramer"} (default) Cramer's V (see \code{\link{cramer.folder}}).
       \item \code{"tschuprow"} Tschuprow's T (\code{\link{tschuprow.folder}}).
       \item \code{"pearson"} Pearson's contingency coefficient (\code{\link{pearson.folder}}).
       \item \code{"phi"} phi (\code{\link{phi.folder}}).
       }
}
  \item{sub.title}{
       string. Subtitle for the graphs (default \code{NULL}).
}
  \item{plot.eigen}{
       logical. If \code{TRUE} (default), the barplot of the eigenvalues is plotted.
}
  \item{plot.score}{
       logical.  If \code{TRUE}, the graphs of new coordinates are plotted. A new graphic device is opened for each pair of coordinates defined by \code{nscore} argument.
}
  \item{nscore}{
       numeric vector. If \code{plot.score = TRUE}, the numbers of the principal coordinates which are plotted. By default, \code{nscore = 1:3}. Its components cannot be greater than \code{nb.factors}. 
}
  \item{filename}{
       string. Name of the file in which the results are saved. By default (\code{filename  = NULL}) they are not saved.
}
   \item{add}{
       logical indicating if an additive constant should be computed and added to the non diagonal dissimilarities such that the modified dissimilarities are Euclidean (default \code{TRUE}; see \code{add} argument of \code{\link{cmdscale}}).
}
  \item{p}{
       integer. Optional. When \code{distance = "lp"} (\eqn{L^p} distance with \eqn{p>2}), \code{p} is the parameter of the distance.
}
}
\details{
    If a folder is given as argument, the \eqn{T} discrete probability distributions \eqn{f_t} corresponding to the \eqn{T} groups of individuals are estimated from observations.
    Then the distances/dissimilarities between the estimated distributions are computed, using the distance or divergence defined by the \code{distance} argument:
    
    If the distance is \code{"l1"}, \code{"l2"} or \code{"lp"}, the distances are computed by the function \code{\link{matddlppar}}.
    Otherwise, it can be computed by \code{\link{matddchisqsympar}} (\code{"chisqsym"}), \code{\link{matddhellingerpar}} (\code{"hellinger"}), \code{\link{matddjeffreyspar}} (\code{"jeffreys"}) or \code{\link{matddjensenpar}} (\code{"jensen"}).

    The association measures are computed accordingly to the value of the parameter \code{association}The computation uses the corresponding function of the package \code{DescTools} (see \code{\link[DescTools]{Assocs}}). Notice that an association measure between a constant variable with and other variable is set to zero. The association measure between each variable with itself is not computed and the diagonal of the returned association matrices is set to \code{NA}.
}
\value{
    Returns an object of class \code{mdsdd}, that is a list including:
    
    \item{inertia}{data frame of the eigenvalues and the percentages of their sum.}
    \item{scores}{data frame of the coordinates along the \code{nb.factors} first principal coordinates.}
    \item{jointp}{list of arrays. The joint probability distribution for each group.}
    \item{margins}{list of two data frames giving respectively:
      \itemize{
        \item{The probability distribution of each variable for each group. Each column of the data frame corresponds to one level of one categorical variable and contains the probabilities of this level in each group.}
        \item{The joint probability distribution of each pair of variables for each group. Each column of the data frame corresponds to one pair of levels of two categorical variables (one level per variable) and contains the probabilities of this pair of levels in each group.}
      }
    }
    \item{associations}{list of \eqn{T} matrices. Each matrix corresponds to a group and gives the pairwise association measures between the \eqn{q} categorical variables.
    }
}
\references{
    Cox, T.F., Cox, M.A.A. (2001). Multidimensional Scaling, second ed. Chapman & Hall/CRC.
    
    Saporta, G. (2006). Probabilit\'es, Analyse des donn\'ees et Statistique. Editions Technip, Paris.
}
\author{
Rachid Boumaza, Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard
}

\seealso{
    \link{print.mdsdd}, \link{plot.mdsdd}, \link{interpret.mdsdd}
}
\examples{
# Example 1 with a folder (10 groups) of 3 factors 
# obtained by converting numeric variables
data(roses)
xr = roses[,c("Sha", "Den", "Sym", "rose")]
xf = as.folder(xr, groups = "rose")
xf = cut(xf, breaks = list(c(0, 5, 7, 10), c(0, 4, 6, 10), c(0, 6, 8, 10)), cutcol = 1:3)
af = mdsdd(xf)
print(af)
print(af$jointp)
print(af$margins[[1]]) # equivalent to print(af$margins$margin1) 
print(af$margins[[2]])
print(af$associations)

# Example 2 with a data frame obtained by converting numeric variables
data(roses)
xr = roses[,c("Sha", "Den", "Sym", "rose")]
xr = cut(xr, breaks = list(c(0, 5, 7, 10), c(0, 4, 6, 10), c(0, 6, 8, 10)), cutcol = 1:3)
ar = mdsdd(xr, group.name = "rose")
print(ar)
print(ar$jointp)
print(ar$margins[[1]]) # equivalent to print(ar$margins$margin1) 
print(ar$margins[[2]])
print(ar$associations)

# Example 3 with a list of 7 arrays
data(dspg)
xl = dspg
mdsdd(xl)
}