# #'Correct p-values of a jointest object for multiple testing (i.e. Selective Inference)
# #'@param mods object of class \code{jointest} (it can be a list of glm or flipscores converted in a \code{jointest} object using \code{as.jointest})
# #'@param method any method implemented in \code{flip::flip.adjust} or a custom function. In the last case it must be a function that uses a matrix as input and returns a vector of adjusted p-values equal to the numbero fo columns of the inputed matrix.
# #'@param tail = 0
# #'@param ... further parameters
# #'@export
# #'@importFrom flip flip.adjust
#' 
#' p.adjust.jointest method for a jointest object.
#' @rdname jointest-method
#' @param object an object of class \code{jointest}.
#' @param method any method implemented in \code{flip::flip.adjust} or a custom function. In the last case it must be a function that uses a matrix as input and returns a vector of adjusted p-values equal to the numbero fo columns of the inputed matrix.
#' @param ... additional arguments to be passed
#' @method  p.adjust jointest
#' @docType methods
#' @importFrom flip flip.adjust
#' @export
#' 
p.adjust.jointest <- function (mods, method = "maxT", tail = 0, ...) 
{
  if(is.character(method)){
    p.adj = flip.adjust(.set_tail(mods$Tspace, tail = tail), 
                            method = method) } 
  else if(is.function(method)){
    p.adj = method(.set_tail(mods$Tspace, tail = tail))
  }
  mods$summary_table$p.adj = p.adj
  mods
}