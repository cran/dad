plot.fhclustd <- function(x, labels = NULL, hang = 0.1, check = TRUE,
                          axes = TRUE, frame.plot = FALSE, ann = TRUE,
                          main = "HCA of probability density functions",
                          sub = NULL, xlab = NULL, ylab = "Height", ...) {
        plot(x$clust, labels = labels, hang = hang, check = check,
             axes = axes, frame.plot = frame.plot, ann = ann,
             main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
        return(invisible(NULL))
}
