interpret.fpcat <-
function(x, nscore=1:3, moment="mean", ...)
{
  times <- x$times
  class(x) <- "fpcad"
  
  ret <- interpret(x, nscore=nscore, moment=moment)

return(invisible(ret))
}
