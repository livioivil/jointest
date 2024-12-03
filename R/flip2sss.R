#' @title flipscores 2-Stage Summary Statistics approach
#' @description This function fits a model based on the provided formula and data, accounting for clusters and summary statistics within the model.
#' @usage flip2sss(formula = NULL, data = NULL, cluster = NULL, 
#' family = "gaussian", summstats_within=NULL, ...)
#' @param formula A formula or a list of formulas. It can be a complete model as.formula or a list of formulas, one for each element produced by the function.
#' @param data The dataset to be used for fitting the model.
#' @param cluster A vector or a formula evaluated on the data that defines the clusters.
#' @param family as in \code{glm}, but given as a character. Not used if argument \code{summstats_within} is not \code{NULL}.
#' @param summstats_within A vector of summary statistics model within the data or a function with argument data.
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
                     ...){
  
  # if(is(cluster,"formula")){
  #   cluster=eval(cluster,data)
  # }
  
  
  ###################
  
  vars_between_within = .get_vars_between_within(formula, data, cluster)  
  
  vars_between=vars_between_within$vars_between
  vars_within=vars_between_within$vars_within
  rm(vars_between_within)
  ## make the second level dataset  
  set_between = unique(unlist(vars_between))
  vars_between_formulas = lapply(vars_between, paste0, collapse = "+")
  vars_between_formulas = paste(names(vars_between_formulas), vars_between_formulas, sep = "~")
  vars_between_formulas = as.list(vars_between_formulas)
  names(vars_between_formulas) = names(vars_between)
  
  #####################
  if(is.null(summstats_within))
    summstats_within=paste0("glm(",formula[[2]],formula[[1]],vars_within,", family=",family,")")
  set_between=c(".cluster",set_between[set_between!="1"])
  data$.cluster=cluster
  
  data2lev <- makedata2lev(data = data, cluster, summstats_within)
   
# data2lev = data %>% 
 #   group_by(data[set_between]) %>%
  #  summarise(as.data.frame(t(coefficients(eval(parse(text=summstats_within))))))
 
  data2lev$.cluster=NULL
  names(data2lev) = gsub("\\W", ".", names(data2lev))
  
  mods = lapply(vars_between_formulas, function(frm) glm(eval(frm, parent.frame()), data = data2lev))
  
  for(i in 1:length(mods)){
    mods[[i]]$call$data = eval(data2lev)
    mods[[i]]$call$formula = eval(as.formula(vars_between_formulas[[i]]))
  }
  
  res = join_flipscores(mods,...)
  # summary(res)
  
  res$summary_table$Coeff = paste(res$summary_table$Coeff, res$summary_table$Model,sep = ":")
  res$summary_table$Coeff = gsub(":\\.Intercept\\.$", "", res$summary_table$Coeff)
  res$summary_table$Coeff = gsub("\\(Intercept\\):", "", res$summary_table$Coeff)
  
  # res$summary_table$Model = "flip2sss"
  colnames(res$Tspace) = paste(res$summary_table$Coeff, res$summary_table$Model,sep = "_model.") 
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
  #clst_vals = unique(cluster) NON SERVE!
  D = model.matrix(formula, data = data)
  
  ## find constant cols within cluster
  const_id = do.call(rbind, by(D, cluster, function(D) as.data.frame(t(apply(D, 2, is.constant)))))
  vars_between_intercept = apply(const_id, 2, all)
  
  #vars_between_intercept
  between_vars=attributes(D)$assign[vars_between_intercept]
  intercept=0%in%between_vars
  between_vars=unique(between_vars)
  between_vars_intercept<-
    between_vars<-attr(terms(formula),"term.labels")[between_vars]
  if(intercept)
    between_vars_intercept=c("1",between_vars_intercept)

  # formula_preds=paste(collapse = "+",between_vars)
  # formula_preds_between_intercept=paste0(formula[[2]],formula[[1]],formula_preds)
  
  # data_intercept=data[,between_vars]
  
  
  
  ############# within variables
  within_vars=attributes(D)$assign[!vars_between_intercept]
  within_vars=unique(within_vars)
  within_vars_all=attr(terms(formula),"term.labels")[within_vars]
  within_vars=within_vars_all
  for(x in between_vars)
    within_vars=gsub(x,"",within_vars)
  within_vars=gsub(":","",within_vars)
  within_vars=unique(within_vars)
  
  ############# others between variables
  vars_between=lapply(within_vars, function(x) {
    temp=within_vars_all[grep(x,within_vars_all)]
    temp=gsub(paste0(x,"(:|)"),"",within_vars_all)
    if(any(temp=="")) temp[temp==""]="1"
    temp
  })
  vars_between=lapply(vars_between,function(x)gsub(":$","",x))
  names(vars_between)=within_vars
  
  temp=list(".Intercept."=between_vars_intercept)
  vars_between=c(temp,vars_between)
  
  
  list(vars_within=within_vars,
       vars_between=vars_between)
}
 
is.constant <- function(x) length(unique(x)) == 1
