crea_multi_var<-function(x,max_cat=3,cats=NULL){
  if(is.null(cats)) cats=2:max_cat
  newx<-data.frame(x=x)
  for(i in 1:length(cats)){
    nome<-paste(deparse(substitute(x)),"_",cats[i],"_cat",sep="")
    newx$temp<-cut(x,breaks=unique(quantile(x,0:cats[i]/cats[i],na.rm=T)),include.lowest = TRUE)
    names(newx)[i+1]<-nome
  }
  names(newx)[1]<-deparse(substitute(x))
  attr(newx,"orig") = names(newx)[1]
  newx 
}

make_models<-function (mod, max_cat = 3, var_exclude = NULL, data_new = NULL, 
                       sets_of_variables = NULL) 
{
  formula = eval(mod$call$formula, parent.frame())
  yvar <- all.vars(formula)[1]
  vars <- all.vars(formula)[-1]
  if (is.null(sets_of_variables)) {
    vars_multi <- vars
    nvar <- length(vars_multi)
    vars_list <- vector("list", nvar)
    vars_list = lapply(1:nvar, function(i) if (vars[i] %in% 
                                               var_exclude) 
      NULL
      else paste(vars_multi[i], "_", 2:max_cat, "_cat", 
                 sep = ""))
  }
  else {
    vars_list = as.list(vars)
    names(vars_list) = vars
    sets_of_variables = sets_of_variables[intersect(names(sets_of_variables), 
                                                    vars)]
    vars_list[names(sets_of_variables)] = sets_of_variables
  }
  combination = expand.grid(vars_list)
  multi_models <- apply(combination, 1, function(trms) {
    mod = update(mod, data = data_new, formula(paste0(paste0(yvar, 
                                                             "~"), paste(trms, collapse = "+"))))
    mod
  })
  attr(multi_models, "combination") <- combination
  multi_models
}

#########################
# multiverse_boostrap<-function (mods, data, var_interest, B = 500, verbatim = TRUE) 
# {
#   n_data <- dim(data)[1]
#   yvar <- all.vars(formula(mods[[1]]))[1]
#   vars <- all.vars(formula(mods[[1]]))[-1]
#   nvar <- length(vars)
#   num_comb <- length(mods)
#   pboot <- matrix(NA, B, num_comb)
#   eboot <- matrix(NA, B, num_comb)
#   pval <- rep(NA, num_comb)
#   eval <- rep(NA, num_comb)
#   pos <- which(vars == var_interest)
#   for (k in 1:num_comb) {
#     formula <- formula(mods[[k]])
#     m1 <- mods[[k]]
#     pval[k] <- coef(summary(m1))[pos + 1, 4]
#     eval[k] <- coef(m1)[pos + 1]
#     data$ytemp <- data[, yvar] - coef(m1)[pos + 1] * (data[, 
#                                                            var_interest])
#     temp <- "ytemp ~ "
#     for (j in 1:nvar) temp <- paste(temp, paste("+", 
#                                                 all.vars(formula(mods[[k]]))[-1][j]))
#     formulak <- formula(temp)
#     set.seed(1)
#     bt <- boot::boot(data, function(d, i) coef(summary(update(mods[[k]], 
#                                                               data = d[i, ], formula = formulak)))[pos + 1, c(1, 
#                                                                                                               4)], R = B, stype = "i")
#     eboot[, k] <- bt$t[, 1]
#     pboot[, k] <- bt$t[, 2]
#     if (verbatim) 
#       show(paste("model", k))
#   }
#   list(pval = pval, eval = eval, pboot = pboot, eboot = eboot, 
#        models = mods)
# }
# 
