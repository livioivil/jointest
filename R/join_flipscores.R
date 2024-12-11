#' @title join tests from multiverse models
#' @description
#' The function allows hypothesis testing across all plausible multiverse models
#' ensuring strong family-wise error rate control.
#' @usage join_flipscores(mods, tested_coeffs = NULL, n_flips = 5000, 
#' score_type = "standardized", statistics = "t", seed=NULL, output_models =TRUE, ...)
#' @param mods list of \code{glm}s or \code{flipscores}-object (or any other object that can be evaluated by \code{flipscores}) 
#' @param tested_coeffs list of the same length of \code{mods}, each element of the list being a vector of  
#' names of tested coefficients. Alternatively, it can be a vector of names of tested coefficients, in this case, the tested coefficients are attributed to all models (when present). 
#' As a last option, it can be \code{NULL}, if so, all coefficients are tested.
#' @param n_flips number of flips, default 5000
#' @param score_type any valid type for \code{flipscores}, \code{"standardized"} is the default. see \code{\link[flipscores]{flipscores}} for more datails 
#' @param statistics "t" is the only method implemented (yet). Any other value will not modify the score (a different statistic will only affect the multivariate inference, not the univariate one).
#' @param seed \code{NULL} by default. Use a number if you wanto to ensure replicability of the results
#' @param output_models \code{TRUE} by default. Should the \code{flipscores} model returned?
#' @param ... any other further parameter.
#' @import flipscores
#' @export
#' @return A \code{jointest} object, i.e., a list containing the following objects: 
#' \describe{
#'  \item{Tspace}{\code{data.frame} where rows represents the sign-flipping transformed (plus the identity one) test and columns the variables.}
#'  \item{summary_table}{\code{data.frame} containing for each model the estimated parameter(s), score(s), std error(s), test(s), partial correlation(s) and p-value(s).} 
#'  \item{mods}{List of \code{glm}s or \code{flipscores} objects.}
#' }
#' @examples
#' library(jointest)
#' set.seed(123)
#' 
#'
#' #EXAMPLE 1: Simulate data:
#' n=20
#' D=data.frame(X=rnorm(n),Z1=rnorm(n),Z2=rnorm(n))
#' D$Y=D$Z1+D$X+rnorm(n)
#' 
#' # Run four glms abd combine it in a list
#' mod1=glm(Y~X+Z1+Z2,data=D)
#' mod2=glm(Y~X+poly(Z1,2)+Z2,data=D)
#' mod3=glm(Y~X+poly(Z1,2)+poly(Z2,2),data=D)
#' mod4=glm(Y~X+Z1+poly(Z2,2),data=D)
#' mods=list(mod1=mod1,mod2=mod2,mod3=mod3,mod4=mod4)
#' 
#' # flipscores jointly on all models
#' res=join_flipscores(mods,n_flips = 1000)
#' summary(combine(res))
#' summary(combine(res, by="Model"))
#' summary(combine_contrasts(res))
#' 
#' #Simulate multivariate (50) bionomial responses 
#' set.seed(123)
#' n=30
#' D=data.frame(X=rnorm(n),Z=rnorm(n))
#' Y=replicate(50,rbinom(n,1,plogis(.5*D$Z+.5*D$X)))
#' colnames(Y)=paste0("Y",1:50)
#' D=cbind(D,Y)
#' mods=lapply(1:50,function(i)eval(parse(text=
#' paste(c("glm(formula(Y",i,"~X+Z),data=D,family='binomial')"),collapse=""))))
#' # flipscores jointly on all models
#' res=join_flipscores(mods,n_flips = 1000,tested_coeffs ="X")
#' summary(res)
#' res=p.adjust(res)
#' summary(res)
#' # Compute lower bound for the true discovery proportion. See packages pARI and sumSome
#' # install.packages("sumSome")
#' # install.packages("pARI")
#' # library(sumSome)
#' # library(pARI)
#' # pARI returns a lower bound equals 0.24, i.e., at least 24% of the models
#' # have a significant effect related to X
#' # pARI::pARI(ix = c(1:50),pvalues = t(jointest:::.t2p(res$Tspace)),family = "simes",delta = 9)$TDP
#' # sumSome returns a lower bound equals 0.42, i.e., at least 42% of the models
#' # have a significant effect related to X
#' # sumSome::tdp(sumSome::sumStats(G = as.matrix(res$Tspace)))


join_flipscores <- function(mods, tested_coeffs = NULL, n_flips = 5000, 
                            score_type = "standardized", 
                            statistics = "t", seed=NULL, output_models = TRUE, ...) 
{
  if(!is.null(seed)) set.seed(seed)
  
  for(i in 1:length(mods))
     mods[[i]]$call$data=eval(mods[[i]]$call$data, parent.frame())
    
  names(mods) = .set_mods_names(mods)
  if (is.null(tested_coeffs)) {
     tested_coeffs = .get_all_coeff_names_list(mods)
   }
  if (!is.list(tested_coeffs)) {
    temp = .get_all_coeff_names_list(mods)
    tested_coeffs = lapply(temp, function(nms) intersect(tested_coeffs, 
                                                         nms))
  }
  
  
  n_obs=sapply(mods, function(mod) length(mod$y))
  n_obs=max(n_obs)
  
  mods_names=names(mods)
  
  FLIPS=make_flips(n_obs=n_obs,n_flips=n_flips)
    mods = lapply(1:length(mods), function(i) {
      temp = flipscores(formula = eval(mods[[i]],parent.frame()), score_type = score_type, 
                        flips = eval(FLIPS), to_be_tested = tested_coeffs[[i]],
                        output_flips=FALSE,nobservations=n_obs
      )
      if (statistics %in% c("t")) 
        if (score_type == "effective" || score_type == 
            "orthogonalized") {
          sumY2s = colSums(temp$scores^2)
          n = nrow(temp$scores)
          tt = sapply(1:length(sumY2s), function(i) flipscores:::.sum2t(temp$Tspace[, 
                                                                                    i], sumY2s[i], n))
          colnames(tt) = colnames(temp$Tspace)
          temp$Tspace = tt
        }
      temp$summary_table=.get_summary_table_from_flipscores(temp)
      temp
    })
    
  if(is.null(mods_names)){
    names(mods)=paste0("mod",1:length(mods))
  } else
    names(mods) = mods_names
    
    
    #####################
    
    # 
    # i=1
    # assign=attr(model.matrix(mods[[i]]),"assign")
    # coeff_names=names(coefficients(mods[[i]]))
    # if(attr(terms(mods[[i]]),"intercept")==1) 
    #   term.labels = c("(Intercept)",attr(terms(mods[[i]]),"term.labels")) else 
    #     term.labels = attr(terms(mods[[i]]),"term.labels")
    # unique_assign_id=unique(assign)
    # temp=data.frame(coeff_names=coeff_names,
    #                 assign=assign,
    #                 term.labels=unlist(sapply(1:length(term.labels),function(i) rep(term.labels[i],sum(assign==unique_assign_id[i])))),
    #                 Model =mods_names[i]
    # )  
    # temp=temp[temp$coeff_names==tested_coeffs[[i]],]
    # assign_var_orig=temp
    # 
    # if(length(mods)>1)
    #   for(i in 2:length(mods)){
    #     assign=attr(model.matrix(mods[[i]]),"assign")+1+max(assign_var_orig$assign)
    #     coeff_names=names(coefficients(mods[[i]]))
    #     if(attr(terms(mods[[i]]),"intercept")==1) 
    #       term.labels = c("(Intercept)",attr(terms(mods[[i]]),"term.labels")) else 
    #         term.labels = attr(terms(mods[[i]]),"term.labels")
    #     unique_assign_id=unique(assign)
    #     temp=data.frame(coeff_names=coeff_names,
    #                     assign=assign,
    #                     term.labels=unlist(sapply(1:length(term.labels),function(i) rep(term.labels[i],sum(assign==unique_assign_id[i])))),
    #                     Model =mods_names[i]
    #     )  
    #     assign_var_orig=rbind(assign_var_orig,temp[temp$coeff_names==tested_coeffs[[i]],])
    #   }
    
  out=list(Tspace=.get_all_Tspace(mods),
           summary_table=.get_all_summary_table(mods),
           mods=mods,
           call = match.call())
  class(out) <- unique(c("jointest", class(out)))
  out
}
