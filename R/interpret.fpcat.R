interpret.fpcat <-
function(x, nscore=1, moment=c("mean", "sd", "var", "cov", "cor", "skewness", "kurtosis"), ...)
{
  times <- x$times
  class(x) <- "fpcad"
  
  ret <- interpret(x, nscore=nscore, moment=moment)

return(invisible(ret))
}
