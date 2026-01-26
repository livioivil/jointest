#' @title flipscores 2-Stage Summary Statistics approach
#' @description This function fits a model based on the provided formula and data, accounting for clusters and summary statistics within the model as described in 
#' A Andreella, J Goeman, J Hemerik, L Finos (2025) Robust Inference for Generalized Linear Mixed Models: A “Two-Stage Summary Statistics” Approach Based on Score Sign Flipping. Psychometrika, 1-23.
#' @usage flip2sss(formula = NULL, data = NULL, cluster = NULL, 
#' family = "gaussian", summstats_within=NULL, n_flips = 5000, flips = NULL, ...)
#' @param formula A formula or a list of formulas. It can be a complete model as.formula or a list of formulas, one for each element produced by the function.
#' @param data The dataset to be used for fitting the model.
#' @param cluster A vector or a formula evaluated on the data that defines the clusters.
#' @param family as in \code{glm}, but given as a character. Not used if argument \code{summstats_within} is not \code{NULL}.
#' @param summstats_within A vector of summary statistics model within the data or a function with argument data.
#' @param n_flips The number of random flips of the score contributions. Overwritten with the \code{nrow(flips)} when \code{flips} is not \code{NULL} (see parameter \code{flips} for more details).
#' When \code{n_flips} is equal or larger than the maximum number of possible flips (i.e. n^2), all possible flips are performed.
#' @param flips matrix fo +1 or -1, the matrix has \code{n_flips} rows and n (number of observations) columns
#' @param ... Other arguments passed to the \code{\link[flipscores]{flipscores}} function.
#' @export
#' @return A \code{jointest} object, i.e., a list containing the following objects: 
#' \describe{
#'  \item{Tspace}{\code{data.frame} where rows represents the sign-flipping transformed (plus the identity one) test and columns the variables.}
#'  \item{summary_table}{\code{data.frame} containing for each second-step covariate the estimated parameter, score, std error, test , partial correlation and p-value.} 
#'  \item{mods}{List of \code{glm} objects, i.e., first-step \code{glm} objects}
#' }
#' @examples
#' library(jointest)
#' set.seed(123)
#' # Simulate data
#' N=20
#' n=rpois(N,20)
#' reff=rep(rnorm(N),n)
#' 
#' D=data.frame(X1=rnorm(length(reff)),
#'              X2=rep(rnorm(N),n),
#'              Grp=factor(rep(rep(LETTERS[1:3],length.out=N),n)),
#'              Subj=rep(1:N,n))
#' D$Y=rbinom(n=nrow(D),prob=plogis( 2*D$X1 * (D$Grp=="B") +  2*D$X2+reff),size=1)
#' 
#' # model of interest formula <- Y ~ Grp * X1 + X2
#' # clusters structure defined by cluster <- factor(D$Subj)
#' # The 2-Stage Summary Statistics via flipscore: 
#' res <- flip2sss(Y ~ Grp * X1 + X2, data=D, 
#'                cluster=D$Subj, family="binomial")
#' summary(res)
#' # This is an ANOVA-like overall test:
#' summary(combine(res))
#' # This is an ANOVA-like test:
#' summary(combine_contrasts(res))
#' 
#' # An alternative and more flexible definition of the model:
#' # Define the summary statistics (here we propose the glm with firth correction 
#' # from the logistf package)
#' summstats_within <- 'logistf::logistf(Y ~ X1, family = binomial(link = "logit"),
#' control=logistf::logistf.control(maxit=100))'
#' # however also the classic glm function can be used:
#' #summstats_within <- 'glm(Y ~ X1, family = binomial(link = "logit"))'
#' 
#' # Then, compute the 2-Stage Summary Statistics approach
#' # specifying the summary statistics (within cluster/subject)
#' res <- flip2sss(Y ~ Grp * X1 + X2, data=D, cluster=D$Subj, 
#'                    summstats_within=summstats_within)
#' summary(res)
#' 
#' # We can also combine the tests:
#' # Overall:
#' summary(combine(res))
#' # This is similar to an ANOVA test:
#' summary(combine_contrasts(res))
#' @import flipscores
#' @author Livio Finos, Angela Andreella
#' @seealso \code{\link{combine_contrasts}}, \code{\link{combine}}
#'  
flip2sss <- function(formula=NULL,
                     data=NULL,
                     cluster=NULL,
                     family="gaussian",
                     summstats_within=NULL,
                     n_flips = 5000,
                     flips = NULL,
                     ...){
  
  if(is(cluster,"formula")){
     cluster=model.frame(cluster,data)
   }
  
  ###################
  
  vars_between_within = .get_vars_between_within(formula, data, cluster)  
  
  vars_within=vars_between_within$vars_within
  pred_vars_between_dummy=vars_between_within$pred_vars_between_dummy
  data=vars_between_within$data
  cluster=vars_between_within$cluster
  
  rm(vars_between_within)
  
  vars_between_formulas_dummy = lapply(pred_vars_between_dummy, paste0, collapse = "+")
  vars_between_formulas_dummy = paste(names(vars_between_formulas_dummy), vars_between_formulas_dummy, sep = "~")
  vars_between_formulas_dummy = as.list(vars_between_formulas_dummy)
  names(vars_between_formulas_dummy) = names(pred_vars_between_dummy)
  #####################
  
  if(is.null(summstats_within)){
    summstats_within=paste0("glm(",formula[[2]],formula[[1]],paste(collapse ="+",vars_within),", family=",family,")")
  } 
  model.matrix.between=model.matrix(formula,data)[,unique(unlist(pred_vars_between_dummy)),drop=FALSE]
  data2lev <- makedata2lev(data = data, cluster, 
                           summstats_within,model.matrix.between)

  mods = lapply(vars_between_formulas_dummy, function(frm) glm(eval(frm, parent.frame()), data = data2lev))
  
  for(i in 1:length(mods)){
    mods[[i]]$call$data = eval(data2lev)
    mods[[i]]$call$formula = eval(as.formula(vars_between_formulas_dummy[[i]]))
  }
  res = join_flipscores(mods, flips = flips, ...)
  

  res$summary_table$coefficient = paste(res$summary_table$coefficient, res$summary_table$model,sep = ":")
  res$summary_table$coefficient = gsub(":\\.Intercept\\.$", "", res$summary_table$coefficient)
  res$summary_table$coefficient = gsub("\\(Intercept\\):", "", res$summary_table$coefficient)
  
  colnames(res$Tspace) = paste(res$summary_table$coefficient, res$summary_table$model,sep = "_model.") 
  res$mods=mods
  res
}

######################
.expand_form <- function(FUN){
  
  out <- reformulate(labels(terms(FUN)), FUN[[2]])
  out
}
######################
.get_vars_between_within <- function(formula, data, cluster){
  formula=.expand_form(formula)
  
  idnas=which(is.na(data),arr.ind = TRUE)
  if(nrow(idnas)>0){
    idnas=unique(idnas[,1])
    data=data[-idnas,,drop=FALSE]
    cluster=cluster[-idnas]
    }
  
  D = model.matrix(formula, data = data)
  
  ## find constant cols within cluster
  const_id = do.call(rbind, by(D, cluster, function(D) as.data.frame(t(apply(D, 2, is.constant)))))
  vars_between_intercept_id = apply(const_id, 2, all)
  
  ########## vars between
  between_vars=attributes(D)$assign[vars_between_intercept_id]
  intercept=0%in%between_vars
  between_vars=unique(between_vars)
  between_vars_intercept<-
    between_vars<-attr(terms(formula),"term.labels")[between_vars]
  if(intercept)
    between_vars_intercept=c("1",between_vars_intercept) else
      between_vars_intercept=c("0",between_vars_intercept)

    # vars between
  between_dummy_vars_intercept=names(vars_between_intercept_id)[vars_between_intercept_id]
#  between_dummy_vars_intercept=gsub("^\\(Intercept\\)$","1",between_dummy_vars_intercept)
  id_intercept=grep("^\\(Intercept\\)$",between_dummy_vars_intercept)
  if(length(id_intercept)>0)
    between_dummy_vars_intercept=between_dummy_vars_intercept[
    -id_intercept]
  
  
  
  # ############# within variables
  within_vars=attributes(D)$assign[!vars_between_intercept_id]
  within_vars=unique(within_vars)
  within_vars_all=attr(terms(formula),"term.labels")[within_vars]
  within_vars=within_vars_all
  for(x in between_vars)
    within_vars=gsub(x,"",within_vars)
  within_vars=gsub(":$","",within_vars)
  within_vars=gsub("^:","",within_vars)
  within_vars=unique(within_vars)
  
  within_dummy_vars_all=names(vars_between_intercept_id)[!vars_between_intercept_id]
  within_dummy_vars=within_dummy_vars_all
  for(x in between_dummy_vars_intercept)
    within_dummy_vars=gsub(paste0("(^|:)",x,"($|:)"),"",within_dummy_vars)
  within_dummy_vars=gsub(":$","",within_dummy_vars)
  within_dummy_vars=gsub("^:","",within_dummy_vars)
  within_dummy_vars=unique(within_dummy_vars)
  
   
  ############# DUMMY predictor between variables for within coefficients
  pred_vars_between_dummy=lapply(within_dummy_vars, function(x) {
    # temp=within_vars_all[grep(x,within_vars_all)]
    x_is_in=grep(paste0("(:|^)",x,"(:|$)"),within_dummy_vars_all)
    temp=gsub(paste0(x,"(:|)"),"",within_dummy_vars_all[x_is_in])
    temp=gsub(":$","",temp)
    if(any(temp=="")) temp=temp[temp!=""]
    #setdiff(temp,within_dummy_vars)
    temp
  })
  #vars_between=lapply(vars_between,function(x)gsub(":$","",x))
  names(pred_vars_between_dummy)=within_dummy_vars
  
  if(intercept){
    temp=list(".Intercept."=between_dummy_vars_intercept)
    pred_vars_between_dummy=c(temp,pred_vars_between_dummy)
  }
  
  ###################
  list(vars_within=within_vars,
       #pred_vars_between=pred_vars_between,
       #vars_within_dummy=within_dummy_vars_intercept,
       pred_vars_between_dummy=pred_vars_between_dummy,
       data=data,
       cluster=cluster)
  }
 
is.constant <- function(x) length(unique(x)) == 1

