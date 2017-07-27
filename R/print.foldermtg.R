print.foldermtg <- function(x, classes = TRUE, description = FALSE, features = TRUE,
  topology = FALSE, coordinates = FALSE, ...) {
  
  which.print <- which(names(x) == "mtg.header")
  if (classes) which.print <- c(which.print, which(names(x) == "classes"))
  if (description) which.print <- c(which.print, which(names(x) == "description"))
  if (features) which.print <- c(which.print, which(names(x) == "features"))
  if (topology) which.print <- c(which.print, which(names(x) == "topology"))
  if (coordinates) which.print <- c(which.print, which(names(x) == "coordinates"))
  which.print <- c(which.print,
      which(!(names(x)) %in% c("classes", "description", "features",
      "topology", "coordinates")))
  print(x[which.print], ...)
}