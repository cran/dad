print.mdsdd <- function(x, joint = FALSE, margin1 = FALSE, margin2 = FALSE, association = FALSE, ...) {
  cat("group variable: ",x$group, "\n")
  cat("variables: ", x$variables, "\n")
  cat("---------------------------------------------------------------\n")
  cat("inertia\n"); print(x$inertia, digits=3, ...)
  #cat("---------------------------------------------------------------\n")
  #cat("contributions\n"); print(x$contributions, ...)
  #cat("---------------------------------------------------------------\n")
  #cat("qualities\n"); print(x$qualities, ...)
  cat("---------------------------------------------------------------\n")
  cat("coordinates\n"); print(x$scores, ...)
  if (joint) {
    cat("---------------------------------------------------------------\n")
    cat("Joint probability distribution\n"); print(x$jointp, ...)
  }
  if (margin1) {
    cat("---------------------------------------------------------------\n")
    cat("Marginal probability distribution\n"); print(x$margins$margin1, ...)
  }
  if (margin2) {
    cat("---------------------------------------------------------------\n")
    cat("Marginal probabilities per combination of 2 variables\n"); print(x$margins$margin2, ...)
  }
  if (association) {
    cat("---------------------------------------------------------------\n")
    cat("association measures per couple of variables\n"); print(x$association, ...)
  }
  return(invisible(x))
}