interpret.fpcat <-
function(x, nscore=1:3, moment=c("mean", "sd", "var", "cov", "cor", "skewness", "kurtosis", "all"), ...)
{
  times <- x$times
  class(x) <- "fpcad"
  
  ret <- interpret(x, nscore=nscore, moment=moment)

return(invisible(ret))
}
