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
    cbind(Model=names(mods)[i],mods[[i]]$summary_table)
  })
  res=do.call(rbind,res)
  rownames(res)=NULL
  res
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
