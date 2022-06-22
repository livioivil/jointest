########### COMBINING FUNCTIONS
################################
.get_all_Tspace <- function(mods){
  # dplyr::bind_cols(lapply(mods, function(md) md$Tspace))
  tab=mods[[1]]$Tspace
  for (i in 2:length(mods)){
    tab=cbind(tab,mods[[i]]$Tspace)
  }
  tab
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

.set_comb_names_in_summary_table <- function(combs,comb_names){
  res=lapply(1:length(combs), function(i) {
    combs[[i]]$summary_table=cbind(Coeff=comb_names[i],combs[[i]]$summary_table)
    combs[[i]]
  })
  res
}

.set_comb_names_in_Tspace <- function(combs,comb_names){
  res=lapply(1:length(combs), function(i) {
    colnames(combs[[i]]$Tspace)=comb_names[i]
    combs[[i]]
  })
  res
}

###################################
get_head_flip_out <- function(x){
  if(length(grep("Negative Binomial",x$family$family))==0)
  {paste("Flip Score Test: 
         score_type =",x$score_type,
         ", n_flips =",x$n_flips,"\n")}
  else 
    paste("Flip Score Test: 
          score_type =",x$score_type,
          ", n_flips =",x$n_flips,
          ", theta =",round(x$theta,digits=5),"\n")
}
