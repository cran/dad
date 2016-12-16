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
analysis, multidimensional scaling, 
discriminant analysis...) for such probability densities.  
}

\details{
\tabular{ll}{
Package: \tab dad\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2016-12-16\cr
License: \tab GPL-2\cr
}
To cite \code{dad}, use \code{citation("dad")}.

The main functions are: \itemize{
\item{\code{\link{fpcad}}: functional principal component analysis (PCA) of probability densities,}
\item{\code{\link{print.fpcad}} and \code{\link{plot.fpcad}}: the \code{print} and \code{plot} methods for objects of class \code{fpcad},}
\item{\code{\link{interpret.fpcad}}: the \code{interpret} method for objects of class \code{fpcad}; helps the user to interpret the scores in terms of moments,}
\item{\code{\link{fmdsd}}: multidimensional scaling applied to probability densities,}
\item{\code{\link{print.fmdsd}} and \code{\link{plot.fmdsd}}: the \code{print} and \code{plot} methods for objects of class \code{fmdsd},}
\item{\code{\link{interpret.fmdsd}}: the \code{interpret} method for objects of class \code{fmdsd}; helps the user to interpret the scores in terms of moments,}
\item{\code{\link{fdiscd.misclass}}: discriminant analysis of probability densities in order to compute the misclassification ratio with the one-leave-out method,}
\item{\code{\link{fdiscd.predict}}: discriminant analysis of probability densities in order to predict the class (synonymous with cluster, not to be confused with the class attribute of an R object) of each probability density whose class is unknown.}
}
We also introduce a class of object, \code{folder}, in order to provide a tool for the manipulation of collections of data frames: \itemize{
\item{\code{\link{folder}}: creation of an object of class \code{folder}.}
\item{\code{\link{mean.folder}}, \code{\link{var.folder}}, \code{\link{cor.folder}}, \code{\link{skewness.folder}}, \code{\link{kurtosis.folder}}: computation of some statistics on the columns of the elements of a folder.}
\item{\code{\link{folderh}} when there is a hierarchical relation between the data frames inside the folder.}
}
}
\author{
Rachid Boumaza,  Pierre Santagostini, Smail Yousfi, Gilles Hunault, Sabine Demotes-Mainard
}
\references{
    Boumaza, R. (1998). Analyse en composantes principales de distributions gaussiennes multidimensionnelles. Revue de Statistique Appliquée, XLVI (2), 5-20.

    Boumaza, R., Yousfi, S., Demotes-Mainard, S. (2015). Interpreting the principal component analysis of multivariate density functions. Communications in Statistics - Theory and Methods, 44 (16), 3321-3339.
    
    Boumaza, R. (2004). Discriminant analysis with independently repeated multivariate measurements: an \eqn{L^2} approach. Computational Statistics & Data Analysis, 47, 823-843.
    
    Delicado, P. (2011). Dimensionality reduction when data are density functions. Computational Statistics & Data Analysis, 55, 401-420.
    
    Rudrauf, J.M., Boumaza, R. (2001). Contribution à l'étude de l'architecture médiévale: les caractéristiques des pierres à bossage des châteaux forts alsaciens. Centre de Recherches Archéologiques Médiévales de Saverne, 5, 5-38.

    Yousfi, S., Boumaza, R., Aissani, D., Adjabi, S. (2014). Optimal bandwith matrices in functional principal component analysis of density function. Journal of Statistical Computation and Simulation, 85 (11), 2315-2330.   
}
