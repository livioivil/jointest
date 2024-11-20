
p.adjust <- function(p, ...) {
  UseMethod("p.adjust")
}
########### COMBINING FUNCTIONS
################################
.get_all_Tspace <- function(mods){
  # dplyr::bind_cols(lapply(mods, function(md) md$Tspace))
  tab=lapply(mods,function(x){
    if(is.null(x$Tspace)){stop("At least one Tspace is missing")}
    x$Tspace})
  nms = sapply(tab,colnames)
  tab=do.call(cbind,tab)
  colnames(tab)=nms
  if(is.null(colnames(tab))) colnames(tab)=paste0("S", 1:ncol(tab))
  
   tab
}


.get_all_summary_table <- function(mods){
  res=lapply(1:length(mods), function(i) {
    cbind(Model=names(mods)[i],
          mods[[i]]$summary_table)
  })
  res=do.call(rbind,res)
  rownames(res)=NULL
  res
}

.get_summary_table_from_flipscores <- function(object){
  tab = as.data.frame(summary(object)$coefficients)
  tab = tab[!is.na(tab[, "Score"]), ]
  colnames(tab)[ncol(tab)]="p"
  
  mm=model.matrix(object)
  .assign=attr(mm,"assign")
  .assign=.assign[dimnames(mm)[[2]]%in%rownames(tab)]
  
  tab = cbind( .assign=.assign,
               Coeff = rownames(tab), 
               tab)
}


# .get_all_Tspace <- function(mods){
#   # dplyr::bind_cols(lapply(mods, function(md) md$Tspace))
#   temp=lapply(mods, function(md) md$Tspace)
#   data.frame(temp)
# }


.get_all_tested_coeffs_names <- function(mods){
  sapply(mods, function(md)colnames(md$Tspace))
}

.get_all_coeff_names_vect <- function(mods){
  sapply(mods, function(md)
    names(md$coefficients))
}

.get_all_coeff_names_list <- function(mods){
  lapply(mods, function(md)
    names(md$coefficients))
}

.get_all_sumScore2 <- function(mods){
  sapply(mods, function(md)
    sum(md$scores^2))
}

.set_mods_names <- function(mods,force=FALSE){
  if((is.null(names(mods)))|force){
    paste0("Model",1:length(mods))
    } else
      names(mods)
}

# .set_comb_names_in_summary_table <- function(combs,comb_names){
#   res=lapply(1:length(combs), function(i) {
#     combs[[i]]$summary_table=cbind(Coeff=comb_names[i],combs[[i]]$summary_table)
#     combs[[i]]
#   })
#   res
# }

.set_comb_names_in_Tspace <- function(combs,comb_names){
  res=lapply(1:length(combs), function(i) {
    colnames(combs[[i]]$Tspace)=comb_names[i]
    combs[[i]]
  })
  res
}

#library(stringr)

# Trova il pattern comune
.find_common_pattern <- function(strings) {
  split_strings <- strsplit(strings, "")
  common <- Reduce(function(x, y) {
    sapply(seq_along(x), function(i) if (x[i] == y[i]) x[i] else "")
  }, split_strings)
  
  paste0(common, collapse = "")
}

###########################################################################
##############################From flipscores##############################
###########################################################################

.make_flips <- function(n_obs,n_flips,id=NULL){
  if(is.null(id)){  
    flips = matrix(3-2*sample(2,(n_flips)*n_obs,replace=TRUE),n_flips,n_obs)
  } else {
    unique_id=unique(id)
    n_id=length(unique_id)
    temp = matrix(3-2*sample(2,(n_flips)*n_id,replace=TRUE),n_flips,n_id)
    flips=matrix(NA,n_flips,n_obs)
    for(i in 1:n_id)
      flips[,id==unique_id[i]]=temp[,i]
    # for(i in unique(id))
    #   for(j in which(id==i)) 
    #     flips[,j]=temp[,i]
  }
  flips
}

#transform sum stat into t stat
.sum2t <- function(stat,sumY2,n){
  # sumY2=sum(Y^2,na.rm = TRUE)
  # n=sum(!is.na(Y))
  # print(sumY2)
  # print(stat)
  # stat0=stat
  sumY2=sumY2*(n**0.5)
  # if(any((sumY2-(stat^2)/n)*(n/(n-1))<0)) browser()
  stat=stat/sqrt((sumY2-(stat^2)/n)*(n/(n-1)))
  # print(stat)
  # if(any(is.na(stat))) browser()
  stat
}

.flip_test_no_pval<- function(scores,
                              flips=NULL,
                              n_flips=NULL,
                              .score_fun,
                              output_flips=FALSE,
                              seed=NULL,
                              precompute_flips=TRUE,
                              ...){
  
  
  ##########################################
  
  #      browser()
  n_obs=nrow(scores)
  Tobs=  .score_fun(rep(1,n_obs),scores)
  #      set.seed(seed)
  if(!is.null(flips)){
    #  browser()
    Tspace=as.vector(c(Tobs,
                       sapply(1:(n_flips-1),
                              function(i).score_fun(flips[i,],scores))))
    
  } else {
    set.seed(seed)
    Tspace=as.vector(c(Tobs,replicate(n_flips-1,{
      .score_fun(sample(c(-1,1),n_obs, replace = T),scores)
    })))
  }
  
  return(Tspace)
}

makedata2lev <- function(data, cluster, summstats_within) { 
  
  groups <- unique(cluster) 
  results <- function(x) { 
    group_data <- data[cluster == x,] 
    stats <- eval(parse(text = summstats_within), envir = group_data) 
    coef_df <- as.data.frame(t(coefficients(stats))) 
    group_data <- group_data[,!(names(group_data) %in%(with(attributes(terms(stats)), as.character(variables[response+1]))
    ))]
    coef_df<- cbind(group_data[setdiff(names(group_data),names(coef_df))],coef_df)
    return(coef_df)
  } 
  result_df <- do.call(rbind, lapply(groups, function(x) unique(results(x))) )
  rownames(result_df) <- NULL 
  
  return(result_df) 
  } 
