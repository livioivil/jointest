#' @importFrom stats qnorm
#' @importFrom stats median
#' @importFrom stats as.formula
#' @importFrom stats coefficients
#' @importFrom stats formula
#' @importFrom stats glm
#' @importFrom stats model.matrix
#' @importFrom stats quantile
#' @importFrom stats reformulate
#' @importFrom stats terms
#' @importFrom stats update

npc <- function (Tspace, comb_funct = "Fisher", tail = 0) 
{
  if(is.character(comb_funct)){
    implemented_comb_funcs=c("mean", "median","Fisher", 
                           "Liptak", "Stoufer", 
                           "Tippet", "minp","maxT","Mahalanobis")
    comb_funct=match.arg(comb_funct,implemented_comb_funcs)
    if (comb_funct %in% c("mean", "median"))
      Tspace = .set_tail(Tspace, tail = tail)
    if (comb_funct %in% c("Fisher", 
                            "Liptak", "Stoufer", 
                            "Tippet", "minp"))
    Tspace = .t2p(Tspace, tail = 1)
  
    if (comb_funct == "minp") 
      Tspace = -Tspace
    if (comb_funct == "mean") {
      Tspace = rowMeans(Tspace)
      Tspace = .set_tail(Tspace, tail = tail)
    }
    else if (comb_funct %in% c("minp", "maxT")) {
      Tspace = .rowMax(Tspace)
    }
    else if (comb_funct == "median") {
      Tspace = .rowMedians(Tspace)
      Tspace = .set_tail(Tspace, tail = tail)
    }
    else if (comb_funct == "Fisher") {
      Tspace = rowSums(.comb_funct_fisher(Tspace))
    }
    else if (comb_funct %in% c("Liptak", "Stoufer")) {
      Tspace = rowSums(.comb_funct_liptak(Tspace))
    } else if (comb_funct %in% c("Mahalanobis")) {
      Tspace = mahalanobis_npc(Tspace)
    }
  } else if (is.function(comb_funct)) {
    Tspace = comb_funct(Tspace)
    comb_funct = "custom"
  }
  Tspace = matrix(Tspace)
  Tspace
}


.rowMedians <- function(X,...)
  apply(X,1,median,...)

.rowMax <- function(X,...)
  apply(X,1,max,...)

.comb_funct_fisher <- function(p)-log(p)
.comb_funct_liptak <- function(p)-qnorm(p)
#add here other functions..
################################

#######################FROM FLIPSCORES PACKAGE#######################

mahalanobis_npc <- function(permT){
  if(ncol(permT)==0) return(rep(0,nrow(permT)))
  permT=as.matrix(permT)
  dimnames(permT)=NULL
  
  if(ncol(permT)==1) {
    permT=as.vector(permT)
    return(abs(permT)/(sum(permT^2)^.5))}
  #else
  ei=eigen(t(permT)%*%permT)
  ei$vectors=ei$vectors[,ei$values>1E-12,drop=FALSE]
  ei$values=ei$values[ei$values>1E-12]
  dst=rowSums((permT%*%ei$vect%*%diag(ei$val^-.5))^2)
  dst=dst*nrow(permT)
  return(dst)
}

#performs mahalanobis_npc() on selected columns of permT
mahalanobis_npc_multi <- function(ids_list,permT){
  
  ff=function(ids,permT) mahalanobis_npc(permT[,ids,drop=FALSE])
  out=sapply(ids_list,ff,permT)
  (out)
}
