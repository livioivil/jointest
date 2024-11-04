#' (nonparametric) combination of jointest object 
#' @param mods object of class \code{jointest} (it can be a list of glm or flipscores converted in a \code{jointest} object using \code{as.jointest})
#' @param comb_funct  Combining function to be used. Several functions are implemented: "mean", "median", "Fisher", "Liptak", (equal to) "Stoufer", "Tippet", (equal to) "minp", "maxT" (the default). 
#' Alternativelly it can be a custom function that has a matrix as input. The function return a vector of length equal to the number of rows of the input matrix. 
#' @param combined a) if \code{NULL} it combines all coefficients with the same names along models, 
#' b) if a vector of characters, it gives a combination for each stats with these names 
#' @param by vector of characters referring to the column's name in summary_table. Equal to "Coeff" by default.
#' @param tail direction of the alternative hypothesis. It can be "two.sided" (or 0, the default), "less" (or -1) or "greater" (or +1)
#' @export
#' 
combine <- function (mods, comb_funct = "maxT", combined = NULL, by=NULL, tail = 0) 
{
  # names(mods) = .set_mods_names(mods)
  if(is.null(by)) by="Coeff"
  smr=apply(res$summary_table[,by,drop=FALSE],1,paste,collapse="_")    
  if (is.null(combined)) 
    combined=attr(mods$Tspace,"orig_var")
  if (is.null(combined)){
#    combined = list(Overall = 1:ncol(res$Tspace))
  uniq_nm = unique(smr)
  if (length(uniq_nm) %in% c(1,length(smr))) {
    combined = list(Overall = 1:ncol(res$Tspace))
    }  else 
      {
    combined = lapply(uniq_nm, function(nm) which(smr == nm))
    names(combined) = uniq_nm
  }
  }
  if (!is.list(combined)) {
    uniq_nm = combined
    combined = lapply(uniq_nm, function(nm) which(smr == nm))
    names(combined) = uniq_nm
  }
  
  res = lapply(1:length(combined), .npc2jointest, 
               mods = mods, combined = combined, tail = tail, comb_funct = comb_funct)
  names(res) = names(combined)
  res=list(Tspace=.get_all_Tspace(res),summary_table=.get_all_summary_table(res))
  class(res) <- unique(c("jointest", class(res)))
  res
}


###################
combine_factors <- function (mods, comb_funct = "maxT", tail = 0) 
{
  # names(mods) = .set_mods_names(mods)
  combined=attr(mods$Tspace,"orig_var")
  if (is.null(combined)){
    warning('There is not attribute "orig_var" in the object $Tspace' )
  }
  if (!is.list(combined)) {
    uniq_nm = combined
    combined = lapply(uniq_nm, function(nm) which(smr == nm))
    names(combined) = uniq_nm
  }
  
  res = lapply(1:length(combined), .npc2jointest, 
               mods = mods, combined = combined, tail = tail, comb_funct = comb_funct)
  names(res) = names(combined)
  res=list(Tspace=.get_all_Tspace(res),summary_table=.get_all_summary_table(res))
  class(res) <- unique(c("jointest", class(res)))
  res
}

#######################
.npc2jointest <- function (id, mods, combined, tail, comb_funct) 
{
  comb_name = names(combined)[id]
  if (is.null(comb_name)) 
    comb_name = "Combined"
  Tspace = npc(mods$Tspace[, combined[[id]],drop=FALSE], comb_funct = comb_funct, 
               tail = tail)
  colnames(Tspace) = comb_name
  Coeff=mods$summary_table[combined[[id]],"Coeff"]
  Coeff=unique(Coeff)
  if(length(Coeff)>1) Coeff="many"
  summary_table = data.frame(Coeff = Coeff, Stat = comb_funct, 
                             nMods = max(1, length(combined[[id]])), S = Tspace[1], 
                             p = .t2p_only_first(Tspace, tail = 1))
  list(Tspace = Tspace, summary_table = summary_table)
}

