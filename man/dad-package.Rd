\name{dad-package}
\alias{dad-package}
\alias{dad}
\docType{package}
\title{
Three-Way Data Analysis Through Densities}

\description{
The three-way data consists of a set of variables measured on several groups of 
individuals. To each group is associated an estimated probability density 
function. The package provides functional methods (principal component 
analysis, multidimensional scaling, cluster analysis, 
discriminant analysis...) for such probability densities.  
}

\details{
\tabular{ll}{
Package: \tab dad\cr
Type: \tab Package\cr
Version: \tab 4.1.2\cr
Date: \tab 2023-08-28\cr
License: \tab GPL-2\cr
URL: https://forgemia.inra.fr/dad/dad
BugReports: https://forgemia.inra.fr/dad/dad/issues
}
To cite \code{dad}, use \code{citation("dad")}.

The main functions applying to the probability densities are: \itemize{
\item{\code{\link{fpcad}}: functional principal component analysis,}
\item{\code{\link{fpcat}}: functional principal component analysis applied to data indexed according to time,}
\item{\code{\link{fmdsd}}: multidimensional scaling,}
\item{\code{\link{fhclustd}}: hierarchical clustering,}
\item{\code{\link{fdiscd.misclass}}: functional discriminant analysis in order to compute the misclassification ratio with the one-leave-out method,}
\item{\code{\link{fdiscd.predict}}: discriminant analysis in order to predict the class (synonymous with cluster, not to be confused with the class attribute of an R object) of each probability density whose class is unknown,}
\item{\code{\link{mdsdd}}: multidimensional scaling of discrete probability distributions,}
\item{\code{\link{discdd.misclass}}: functional discriminant analysis of discrete probability distributions, in order to compute the misclassification ratio with the one-leave-out method,}
\item{\code{\link{discdd.predict}}: discriminant analysis of discrete probability distributions, in order to predict the class of each probability distribution whose class is unknown,}
}
The above functions are completed by: \itemize{
\item{A \code{print()} method for objects of class \code{fpcad}, \code{fmdsd}, \code{fdiscd.misclass}, \code{fdiscd.predict} or \code{mdsdd}, in order to display the results of the corresponding function,}
\item{A \code{plot()} method for objects of class \code{fpcad}, \code{fmdsd}, \code{fhclustd} or \code{mdsdd}, in order to display some useful graphics attached to the corresponding function,}
\item{A generic function \code{\link{interpret}} that applies to objects of class \code{fpcad} \code{fmdsd} or \code{mdsdd}, helps the user to interpret the scores returned by the corresponding function, in terms of moments (\code{fpcad} or \code{fmdsd}) or in terms of marginal probability distributions (\code{mdsdd}).}
}

We also introduce classes of objects and tools in order to handle collections of data frames: \itemize{
\item{\code{\link{folder}} creates an object of class \code{folder}, that is a list of data frames which have in common the same columns.

The following functions apply to a folder and compute some statistics on the columns of its elements: \code{\link{mean.folder}}, \code{\link{var.folder}}, \code{\link{cor.folder}}, \code{\link{skewness.folder}} or \code{\link{kurtosis.folder}}.}
\item{\code{\link{folderh}} creates an object of class \code{folderh}, that is a list of data frames with a hierarchic relation between each pair of consecutive data frames.}
\item{\code{\link{foldert}} creates an object of class \code{foldert}, that is a list of data frames indexed according to time, concerning the same individuals and variables or not.}
\item{\code{\link{read.mtg}} creates an object of class \code{foldermtg} from an MTG (Multiscale Tree Graph) file containing plant architecture data.}
}
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Sabine Demotes-Mainard with the contributions from Gilles Hunault, Julie Bourbeillon and Besnik Pumo
}
\references{
    Boumaza, R. (1998). Analyse en composantes principales de distributions gaussiennes multidimensionnelles. Revue de Statistique Appliqu?e, XLVI (2), 5-20.

    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
    
    Boumaza, R. (2004). Discriminant analysis with independently repeated multivariate measurements: an \eqn{L^2} approach. Computational Statistics & Data Analysis, 47, 823-843.

    Delicado, P. (2011). Dimensionality reduction when data are density functions. Computational Statistics & Data Analysis, 55, 401-420.
    
    Deza, M.M. and Deza E. (2013). Encyclopedia of distances. Springer.
    
    Pradal, C., Godin, C. and Cokelaer, T. (2023). \href{https://mtg.readthedocs.io/en/latest/user/index.html}{MTG user guide}
    
    Rudrauf, J.M., Boumaza, R. (2001). Contribution à l'étude de l'architecture médiévale: les caractéristiques des pierres à bossage des châteaux forts alsaciens. Centre de Recherches Archéologiques Médiévales de Saverne, 5, 5-38.
    
    Rachev, S.T., Klebanov, L.B., Stoyanov, S.V. and Fabozzi, F.J. (2013). The methods of distances in the theory of probability and statistics. Springer.

    Yousfi, S., Boumaza, R., Aissani, D., Adjabi, S. (2014). Optimal bandwith matrices in functional principal component analysis of density functions. Journal of Statistical Computation and Simulation, 85 (11), 2315-2330.   
}
