########### COMBINING FUNCTIONS
################################
.get_all_Tspace <- function(mods){
  # dplyr::bind_cols(lapply(mods, function(md) md$Tspace))
  tab=lapply(mods,function(x){
    if(is.null(x$Tspace)){stop("At least one Tspace is missing")}
    x$Tspace})
  nms = unlist(sapply(tab,colnames))
  tab=do.call(cbind,tab)
  colnames(tab)=nms
  if(is.null(colnames(tab))) colnames(tab)=paste0("S", 1:ncol(tab))
  
   tab
}


.get_all_summary_table <- function(mods,mods_name=NULL){
  if(is.null(mods_name)) mods_name=names(mods)
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
# .find_common_pattern <- function(strings) {
#   split_strings <- strsplit(strings, "")
#   common <- Reduce(function(x, y) {
#     sapply(seq_along(x), function(i) if (x[i] == y[i]) x[i] else "")
#   }, split_strings)
#   
#   paste0(common, collapse = "")
# }
.find_common_pattern <- function(vettore) {
  if (length(vettore) < 2) {
    return(vettore)
  }
  
  if(length(grep(":",vettore[1]))>0){
    vettore_splt=strsplit(vettore,":")
    pttrns=sapply(1:length(vettore_splt[[1]]),function(i){
      .find_common_pattern (sapply(vettore_splt,function(x) x[i]) ) 
    })
    return(paste(pttrns,collapse=":"))
  }
    
  chars=sapply(vettore,strsplit,"")
  #matrix of all chracters
  charsMat=suppressWarnings(do.call(cbind,chars))
  # ask row-wise if they are all equals
  all_eqs=apply(charsMat,1,function(x)length(unique(x))==1)
  # this is the first different character
  if(all(all_eqs)) {
    common_pattern=vettore[1]
  } else {
    common_pattern=substr(vettore[1],1,max(which.min(all_eqs)-1,1))
  }
  
  
  return(common_pattern)
}

###########################################################################
##############################From flipscores##############################
###########################################################################


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
