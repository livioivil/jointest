#' flip2sss
#'
#' This function fits a model based on the provided formula and data, accounting for clusters and summary statistics within the model.
#'
#' @param formula A formula or a list of formulas. It can be a complete model as.formula or a list of formulas, one for each element produced by the function.
#' @param data The dataset to be used for fitting the model.
#' @param cluster A vector or a formula evaluated on the data that defines the clusters.
#' @param summstats_within A vector of summary statistics model within the data or a function with argument data.
#' @param ... Other arguments passed to the `flipscores` function.
#'
#' @return A jointest object containing the model results. Note that the flipscores models for each coefficient within are also included in the jointest object.
#' @export
#'
#' @examples
#' set.seed(1)
#' N=50
#' n=rpois(N,10)
#' reff=rep(rnorm(N),n)
#' 
#' D=data.frame(X1=rnorm(length(reff)),
#'              X2=rep(rnorm(N),n),
#'              Grp=factor(rep(rep(LETTERS[1:3],length.out=N),n)),
#'              SOGG=rep(1:N,n))
#' D$Y=rbinom(n=nrow(D),prob=dlogis((D$Grp=="B") * D$X1 + D$X2),size=1)
#' 
#' formula <- Y ~ Grp * X1 + X2
#' cluster <- factor(D$SOGG)
#' library(logistf)
#' summstats_within <- 'logistf::logistf(Y ~ X1, family = binomial(link = "logit"),control=logistf::logistf.control(maxit=100))'
#' summstats_within <- 'glm(Y ~ X1, family = binomial(link = "logit"))'
#' res <- flip2sss(formula, D, cluster, summstats_within)
#' summary(res)
#' @import dplyr
#' @import magrittr
#' @author Livio Finos, Angela Andreella
#'  
flip2sss <- function(formula=NULL,
                     data=NULL,
                     cluster=NULL,
                     summstats_within=NULL,
                     ...){
  
  # if(is(cluster,"formula")){
  #   cluster=eval(cluster,data)
  # }
  
  
  ###################
  
  vars_between = .get_sets_vars_between(formula, data, cluster)  
  
  ## make the second level dataset  
  set_between = unique(unlist(vars_between))
  vars_between_formulas = lapply(vars_between, paste0, collapse = "+")
  vars_between_formulas = paste(names(vars_between_formulas), vars_between_formulas, sep = "~")
  vars_between_formulas = as.list(vars_between_formulas)
  names(vars_between_formulas) = names(vars_between)
  
  #####################
  data2lev = data %>% 
    group_by(data[,set_between[-1]]) %>%
    summarise(as.data.frame(t(coefficients(eval(parse(text=summstats_within))))))
  names(data2lev) = gsub("\\W", ".", names(data2lev))
  
  mods = lapply(vars_between_formulas, function(frm) glm(eval(frm, parent.frame()), data = data2lev))
  
  for(i in 1:length(mods)){
    mods[[i]]$call$data = eval(data2lev)
    mods[[i]]$call$formula = eval(as.formula(vars_between_formulas[[i]]))
  }
  
  res = join_flipscores(mods,...)
  # summary(res)
  
  res$summary_table$Coeff = apply(res$summary_table[, 2:1], 1, paste, collapse = ":")
  res$summary_table$Coeff = gsub(":\\.Intercept\\.$", "", res$summary_table$Coeff)
  res$summary_table$Coeff = gsub("\\(Intercept\\):", "", res$summary_table$Coeff)
  
  # res$summary_table$Model = "flip2sss"
  colnames(res$Tspace) = apply(res$summary_table[, 2:1], 1, paste, collapse = "_model.")
  res$mods=mods
  res
}

######################
.get_sets_vars_between <- function(formula, data, cluster){
  clst_vals = unique(cluster) NON SERVE!
  D = model.matrix(formula, data = data)
  
  ## find constant cols within cluster
  const_id = do.call(rbind, by(D, cluster, function(D) as.data.frame(t(apply(D, 2, is.constant)))))
  vars_between_intercept = apply(const_id, 2, all)
  
  terms = colnames(D)#attributes(terms(formula))$term.labels#labels(terms(reformulate(as.character(formula))))
  ids_const = unique(attributes(D)$assign[vars_between_intercept])
  # attributes(D)$assign%in%ids_const
   if(attributes(terms(formula))$intercept==1){
    ids_const = ids_const + 1
    terms = c("1", terms)
   }
  intercept_vars_between_names=names(vars_between_intercept[vars_between_intercept])
  intercept_vars_between_names=gsub("^\\(Intercept\\)$","1",intercept_vars_between_names)
  vars_between_names = list(.Intercept. = intercept_vars_between_names)
  
  cors = suppressWarnings(by(D[, !vars_between_intercept], cluster, cor))
  dim3 = c(dim(cors[[1]]), length(cors))
  cors = array(unlist(cors), dim3)
  cors = apply(cors, c(1, 2), min, na.rm = TRUE)
  cors[is.infinite(cors)]=1
  ei = eigen(cors)
  vars_between_others = lapply(which(ei$values > .1), function(cls) names(vars_between_intercept[!vars_between_intercept])[which(ei$vectors[, cls] != 0)])
  within_coeffs = sapply(vars_between_others, function(preds_vars) Reduce(intersect, strsplit(preds_vars, ":")))  
  
  vars_between_others = lapply(1:length(within_coeffs), function(i) gsub(within_coeffs, "1", vars_between_others[[i]]))
  vars_between_others = lapply(1:length(within_coeffs), function(i) gsub(":1$", "", vars_between_others[[i]]))
  vars_between_others = lapply(1:length(within_coeffs), function(i) gsub("^1:", "", vars_between_others[[i]]))
  
  names(vars_between_others) = within_coeffs
  vars_between_names = c(vars_between_names, vars_between_others)
  vars_between_names
}

is.constant <- function(x) length(unique(x)) == 1
