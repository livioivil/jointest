#'jointest for flipscores objects
#'@param mods list of \code{glm}s (or list of any other object that can be evaluated by flipscores) 
#'@param tested_coeffs is a list of the same length of \code{mods}, each element of the list being a vector of  
#'of names of tested coefficients. Alternatively, it can be a vector of names of tested coefficients, in this case, the tested coefficients are attributed to all models (when present). 
#'As a last option, it can be \code{NULL}, if so, all coefficients are tested.
#'@param n_flips = 5000
#'@param score_type any valid type for \code{flipscores}, \code{"standardized"} is the default. see \code{\link[flipscores]{flipscores}} for more datails 
#'@param statistics "t" is the only method implemented (yet). Any other value will not modify the Score (a different statistic will only affect the multivariate inference, not the univariate one).
#'@param seed \code{NULL} by default. Use a number if you wanto to ensure replicability of the results
#'@param output_models \code{TRUE} by default. Should the \code{flipscores} model returned?
#'@param ... any other further parameter.
#'@export
#'
#'@examples
#'n=20
#'set.seed(123)
#'D=data.frame(X=rnorm(n),Z1=rnorm(n),Z2=rnorm(n))
#'D$Y=D$Z1+D$X+rnorm(n)
#'mod1=glm(Y~X+Z1+Z2,data=D)
#'mod2=glm(Y~X+poly(Z1,2)+Z2,data=D)
#'mod3=glm(Y~X+poly(Z1,2)+poly(Z2,2),data=D)
#'mod4=glm(Y~X+Z1+poly(Z2,2),data=D)
#'mods=list(mod1=mod1,mod2=mod2,mod3=mod3,mod4=mod4)
#'library(jointest)
#'res=join_flipscores(mods)

#'summary(res)
#'summary(jointest::combine(res,by="Coeff"))
#'summary(jointest::combine(res,by="Model"))
#'summary(jointest::combine_contrasts(res))

#'res=jointest:::p.adjust.jointest(res)
#'summary(res)
#'
#'
#'D$Yna=D$Y
#'D$Yna[1:2]=NA
#'mod1=glm(Yna~X+Z1+Z2,data=D)
#'mods=list(mod1=mod1,mod2=mod2,mod3=mod3,mod4=mod4)
#'for(i in 1:length(mods))
#' mods[[i]]$call$data=eval(D)
#'library(jointest)
#'res=join_flipscores(mods, tested_coeffs = "X")
#'summary(res)
#'summary(combine(res))
#'
join_flipscores <- function (mods, tested_coeffs = NULL, n_flips = 5000, score_type = "standardized", 
                             statistics = "t", seed=NULL, output_models, ...) 
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
  
  
 #   require(flipscores)
  n_obs=sapply(mods, function(mod) length(mod$y))
  n_obs=max(n_obs)
  
  mods_names=names(mods)
  
  FLIPS=flipscores:::.make_flips (n_obs=n_obs,n_flips=n_flips)
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
    names(mods)=paste0("mod",1:length(mos))
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
           mods=mods)
  class(out) <- unique(c("jointest", class(out)))
  out
}
