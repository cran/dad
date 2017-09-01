appendtofolderh <- function(fh, df, key, after = FALSE)
# fh   : folderh with tow or more data frames.
# df   : data frame to be added to the folderh fh.
# key  : name of the column of df and the first (if after = FALSE)
#        or last (if after = TRUE) element of fh containing the key.
#        - If after = FALSE: df[, key] and fh[[1]][, key] must be
#          factors with the same levels, and each one of these
#          levels must occur exactly once in fh[[1]][, key].
#        - If after = TRUE: df[, key] and fh[[length(fh)]][, key]
#          must be factors with the same levels, and each one of
#          these levels must occur exactly once in
#          fh[[length(fh)]][, key].
# after: logical.
#        - If FALSE (default), df will be added before the first
#          data frame of fh.
#        - If TRUE, df will be added after the last data frame
#          of fh.
{

  name.fh <- deparse(substitute(fh))
  name.df <- deparse(substitute(df))

  if (!is.folderh(fh))
    stop("fh must be an object of class 'folderh'.")

  if (!(key %in% colnames(df)))
    stop("There is no ", key, " column in ", name.df, " data frame.")

  if (!after) {
    if (!(key %in% colnames(fh[[1]])))
      stop(paste0("There is no ", key, " column in ", paste(names(fh)[1], collapse = ", "),
          " data frame of ", name.fh, "."))
    fh.ret <- c(list(df), fh)
    keys <- c(key, attr(fh, "keys"))
    names(fh.ret)[1] <- name.df
  } else {
    if (!(key %in% colnames(fh[[length(fh)]])))
      stop(paste0("There is no ", key, " column in ", paste(names(fh)[length(fh)],
          collapse = ", "), " data frame of ", name.fh, "."))
    fh.ret <- c(fh, list(df))
    keys <- c(attr(fh, "keys"), key)
    names(fh.ret)[length(fh)+1] <- name.df
  }

  class(fh.ret) <- "folderh"
  attr(fh.ret, "keys") <- keys

  return(invisible(fh.ret))
}
