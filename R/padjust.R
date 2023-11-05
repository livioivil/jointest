#'Correct p-values of a jointest object for multiple testing (FamilyWise Error Rate)
#'@param mods object of class \code{jointest} (it can be a list of glm or flipscores converted in a \code{jointest} object using \code{as.jointest})
#'@param method any method implemented in \code{flip::flip.adjust} or a custom function. In the last case it must be a function that uses a matrix as input and returns a vector of adjusted p-values equal to the numbero fo columns of the inputed matrix.
#'@param tail = 0
#'@param ... further parameters
#'@export
#'@importFrom flip flip.adjust


p.adjust.fwer <- function (mods, method = "maxT", tail = 0, ...) 
{
  Tspace = as.matrix(.get_all_Tspace(mods))
  colnames(Tspace) = paste0("v", 1:ncol(Tspace))
  if(is.character(method)){
    p.adj = flip.adjust(.set_tail(Tspace, tail = tail), 
                            method = method) } 
  else if(is.function(method)){
    p.adj = method(.set_tail(Tspace, tail = tail))
                              
  }
  tmp = lapply(1:length(mods), function(id) {
    mods[[id]]$summary_table$p.adj = p.adj[id]
    mods[[id]]
  })
  names(tmp)=names(mods)
  class(tmp) <- unique(c("jointest", class(tmp)))
  tmp
}