summary.foldermtg <- function(object, ...) {
  # Indices of the data frames containing the vertices with the values
  # of the corresponding features
  i.vertices <- which(!(names(object) %in% c("classes", "description", "features",
      "topology", "coordinates")))
  value <- lapply(object[i.vertices], summary)
  
  return(value)
}