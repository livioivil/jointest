#' @title join tests from multiverse flip2sss models
#' @description
#' The function allows hypothesis testing across all plausible multiverse flip2sss models
#' ensuring strong family-wise error rate control.
#' @usage join_flip2sss(formulas, summstats_within, data, cluster, tested_coeffs = NULL, n_flips = 5000, flips = NULL, seed=NULL, ...)
#' @param formulas A formula or a list of formulas. It can be a (list) of complete models as.formula or a list of formulas
#' @param summstats_within A (list of) vector of summary statistics model within the data or a (list of) function with argument data.
#' @param data The dataset to be used for fitting the model.
#' @param cluster A vector or a formula evaluated on the data that defines the clusters.
#' @param tested_coeffs list of the same length of \code{mods}, each element of the list being a vector of  
#' names of tested coefficients. Alternatively, it can be a vector of names of tested coefficients, in this case, the tested coefficients are attributed to all models (when present). 
#' As a last option, it can be \code{NULL}, if so, all coefficients are tested.
#' @param n_flips number of flips, default 5000
#' @param flips matrix fo +1 or -1, the matrix has \code{n_flips} rows and n (number of observations) columns
#' @param seed \code{NULL} by default. Use a number if you wanto to ensure replicability of the results
#' @param ... any other further parameter.
#' @import flipscores
#' @export
#' @return A \code{jointest} object, i.e., a list containing the following objects: 
#' \describe{
#'  \item{Tspace}{\code{data.frame} where rows represents the sign-flipping transformed (plus the identity one) test and columns the variables.}
#'  \item{summary_table}{\code{data.frame} containing for each model the estimated parameter(s), score(s), std error(s), test(s), partial correlation(s) and p-value(s).} 
#'  \item{mods}{List of \code{glm}s or \code{flipscores} objects.}
#' }
#' @example 
#' N=20
#' n=rpois(N,20)
#' reff=rep(rnorm(N),n)
#' D=data.frame(X1=rnorm(length(reff)),
#'                X2=rep(rnorm(N),n),
#'                Grp=factor(rep(rep(LETTERS[1:3],length.out=N),n)),
#'                Subj=rep(1:N,n))
#'D$Y=rbinom(n=nrow(D),prob=plogis( 2*D$X1 * (D$Grp=="B") +  2*D$X2+reff),size=1)

#'summstats_within <- list('glm(Y ~ X1, family = binomial(link = "logit"))',
#'                         'logistf::logistf(Y ~ X1^2, family = binomial(link = "logit"), control=logistf::logistf.control(maxit=100))')

#'formulas <- list(Y ~ Grp * X1 + X2, Y ~ Grp + X1 + X2,  Y ~ Grp + X1)

#'res <- join_flip2sss(formulas = formulas, summstats_within = summstats_within, 
#'                    data = D, cluster = D$Subj) 

join_flip2sss <- function(formulas, summstats_within, data, cluster, tested_coeffs = NULL, n_flips = 5000, flips = NULL,
                          seed=NULL, ...) 
{
  if(!is.null(seed)) set.seed(seed)
  
  n_mods_within <- length(summstats_within)
  n_mods_between <- length(formulas)
  
  combs <- expand.grid(mods_between = seq(n_mods_between),
                       mods_within = seq(n_mods_within))
  
  n_mods <- nrow(combs)
  
  
  mods <- lapply(seq(n_mods), function(x) 
    flip2sss(formula = formulas[[combs[x, 1]]], data = data,
             cluster = cluster,
             summstats_within = summstats_within[[combs[x,2]]]))
  
  
  for(i in 1:length(mods))
    mods[[i]]$call$data=eval(mods[[i]]$call$data, parent.frame())
  
  names(mods) = .set_mods_names(mods)
  if (is.null(tested_coeffs)) {
    tested_coeffs = .get_all_coeff_names_list_flip2sss(mods)
  }
  if (!is.list(tested_coeffs)) {
    temp = .get_all_coeff_names_list(mods)
    
    tested_coeffs = gsub(" ", "", tested_coeffs)          
    tested_coeffs = lapply(temp, function(nms) intersect(tested_coeffs, 
                                                         gsub(" ", "", nms)))
  }
  
  n_obs_rn = sapply(mods, function(mod) max(as.numeric(nrow(mod$mods[[1]]$model))))
  n_obs_rn = max(n_obs_rn)
  n_obs=sapply(mods, function(mod) length(mod$mods[[1]]$residuals))
  n_obs=max(n_obs,n_obs_rn)
  
  
  mods_names=names(mods)
  if(is.null(flips)){
    FLIPS=make_flips(n_obs=n_obs,n_flips=n_flips)
  }else{
    FLIPS = flips
  }
  mods = lapply(1:length(mods), function(i) {
    
    temp = flip2sss(formula = formulas[[combs[i, 1]]], data = data,
                    cluster = cluster,
                    summstats_within = summstats_within[[combs[i,2]]], ...)
    temp
  })
  
  if(is.null(mods_names)){
    names(mods)=paste0("mod",1:length(mods))
  } else
    names(mods) = mods_names
  
  
  out=list(Tspace=.get_all_Tspace(mods),
           summary_table=.get_all_summary_table(mods),
           mods=mods,
           call = match.call())
  class(out) <- unique(c("jointest", class(out)))
  out
}
