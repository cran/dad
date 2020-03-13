cut.folder <- function(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE,
                       dig.lab = 3L, ordered_result = FALSE, cutcol = NULL, ...) {
  # folder method for cut() generic function:
  # Apply cut() to each variable of each element of a folder
  #
  # Arguments:
  #   - x      :          a folder
  #   - breaks :          a list. Its length is equal to the number of elements of the folder.
  #                       It gives the breaks for each variable of the folder. It can be:
  #                         - a list of numeric vectors: the j-th element corresponds to
  #                           the j-th variable of the folder, and is a vector of two or more
  #                           unique cut points
  #                         - a list of single numbers (greater  or equal to 2).
  #                           Its j-th element gives the number of intervals into which th j-th variable
  #                           of the folder is to be cut.
  #   - labels :         list of character vectors. Its length is equal to the number of elements
  #                      of the folder.
  #                      The j-th element gives the labels for the intervals of the j-th function
  #                      of the folder.
  #                      By default, labels are constructed using "(a,b]" interval notation.
  #                      If labels = FALSE, simple integer codes are returned instead of a factor.
  #   - include.lowest : logical, indicating if an ‘x[i]’ equal to the lowest (or highest, for right = FALSE) ‘breaks’ value should be included.
  #   - right          : logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa.
  #   - dig.lab :      : integer or list of integer which is used when labels are not given.
  #                      It determines the number of digits used in formatting the break numbers.
  #                      If it is a single value, it gives the number of digits for all variables of the folder.
  #                      If it is a list of integers, its length is equal to the number of variables,
  #                      and the j-th element gives the number of digits for the j-th variable of the folder.
  # ordered_result     : logical: should the results be ordered factors?
  
  # Change the folder into a data frame
  x <- as.data.frame(x, stringsAsFactors = TRUE)
  
  # Apply cut_data.frame
  result <- cut.data.frame(x, breaks = breaks, labels = labels, include.lowest = include.lowest,
                           right = right, dig.lab = dig.lab, ordered_result = ordered_result,
                           cutcol = cutcol)
  
  return(as.folder(result))
}
