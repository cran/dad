cramer.folder <- function(xf) {
  # convert xf to a data frame
  x = as.data.frame(xf)
  
  group = x[, "group"] # Factor: the groups
  x = x[, -ncol(x)] # Data frame of the variables
  
  # Apply cramer.data.frame to the data of each group
  return(by(x, INDICES=group, FUN=cramer.data.frame, check = FALSE))
}

tschuprow.folder <- function(xf) {
  # convert xf to a data frame
  x = as.data.frame(xf)
  
  group = x[, "group"] # Factor: the groups
  x = x[, -ncol(x)] # Data frame of the variables
  
  # Apply tschuprow.data.frame to the data of each group
  return(by(x, INDICES=group, FUN=cramer.data.frame, check = FALSE))
}

pearson.folder <- function(xf) {
  # convert xf to a data frame
  x = as.data.frame(xf)
  
  group = x[, "group"] # Factor: the groups
  x = x[, -ncol(x)] # Data frame of the variables
  
  # Apply tschuprow.data.frame to the data of each group
  return(by(x, INDICES=group, FUN=pearson.data.frame, check = FALSE))
}

phi.folder <- function(xf) {
  # convert xf to a data frame
  x = as.data.frame(xf)
  
  group = x[, "group"] # Factor: the groups
  x = x[, -ncol(x)] # Data frame of the variables
  
  # Apply tschuprow.data.frame to the data of each group
  return(by(x, INDICES=group, FUN=pearson.data.frame, check = FALSE))
}
