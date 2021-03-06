print.fpcat <-
function(x, mean.print=FALSE, var.print=FALSE, cor.print=FALSE, 
            skewness.print=FALSE, kurtosis.print=FALSE, digits=2, ...)
{
cat("group variable (observation times): ",x$group, "\n")
cat("variables: ", x$variables, "\n\n")
cat("observation times:\n"); print(x$times)
cat("---------------------------------------------------------------\n")
cat("inertia\n"); print(x$inertia, digits=3, ...)
cat("---------------------------------------------------------------\n")
cat("contributions\n"); print(x$contributions, ...)
cat("---------------------------------------------------------------\n")
cat("qualities\n"); print(x$qualities, ...)
cat("---------------------------------------------------------------\n")
cat("scores\n"); print(x$scores, ...)
# Affichage des moyennes et ?carts-types par groupe (optionnel)
if (mean.print) 
  {n.group <- length(x$means)
   n.var <- length(x$means[[1]])
   Means <- matrix(unlist(x$means), nrow = n.group, ncol = n.var, byrow = TRUE, 
         dimnames = list(NULL, paste("mean", x$variables, sep = ".")))
   Means <- data.frame(group=names(x$means), Means, stringsAsFactors = TRUE)
   colnames(Means)[1]=colnames(x$contributions)[1]
   
   if (n.var > 1) {
     st.dev <- unlist(lapply(lapply(x$variances, diag), sqrt))
   } else {
     st.dev <- unlist(lapply(x$variances, sqrt))
   }
   Means <- data.frame(Means, matrix(st.dev, nrow = n.group, ncol = n.var, byrow = TRUE,
                                     dimnames = list(NULL, paste("sd", x$variables))),
                       stringsAsFactors = TRUE)
   Means <- data.frame(Means, norm = x$norm$norm, stringsAsFactors = TRUE)
   cat("---------------------------------------------------------------\n")
   cat("means, standard deviations and norm by group\n") 
   print(Means, digits=digits, ...)
  }
# Variances per group (optional)
if (var.print) 
  {cat("---------------------------------------------------------------\n")
   cat("variances/covariances by group\n"); print(x$variances, digits=digits, ...)
  }
# Correlation per group (optional)
if (cor.print) 
  {cat("---------------------------------------------------------------\n")
  cat("correlations by group\n"); print(x$correlations, digits=digits, ...)
  }
# Skewness per group (optional)
if (skewness.print) 
  {cat("---------------------------------------------------------------\n")
  cat("skewness coefficients by group\n"); print(x$skewness, digits=digits, ...)
  }
# Kurtosis per group (optional)
if (kurtosis.print) 
  {cat("---------------------------------------------------------------\n")
  cat("kurtosis coefficients by group\n"); print(x$kurtosis, digits=digits, ...)
  }
return(invisible(x))
}
