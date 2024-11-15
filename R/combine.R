#' (nonparametric) combination of jointest object 
#' @param mods object of class \code{jointest} (it can be a list of glm or flipscores converted in a \code{jointest} object using \code{as.jointest})
#' @param comb_funct  Combining function to be used. Several functions are implemented: "mean", "median", "Fisher", "Liptak", (equal to) "Stoufer", "Tippet", (equal to) "minp", "maxT" (the default). 
#' Alternativelly it can be a custom function that has a matrix as input. The function return a vector of length equal to the number of rows of the input matrix. 
#' @param by if \code{NULL} (default), it combines all test statistics.  
#' If a characters, it refers to the column's name of \code{summary_table} (and printed by something like \code{summary(mods)}). 
#' The elements with the same value will be combined. If \code{by} is a vector, the values are defined by row-wise concatenation of the values of the columns in \code{by}.
#' The argument is inactive if \code{by_list} is not \code{NULL}.
#' @param by_list NULL (default) or a list of vectors. For each vector of the list it combines test statistics with position given by the element of the vector. 
#' If the vectors in the list are characters, these refer to names(mods$Tspace).
#' @param tail direction of the alternative hypothesis. It can be "two.sided" (or 0, the default), "less" (or -1) or "greater" (or +1)
#' @export
#' 
combine <- function (mods, comb_funct = "maxT", by = NULL, by_list=NULL, tail = 0) 
{
  if(!is.null(by_list)){
    combined=by_list
  } else    
    { # by_list is not active, let's use by
    
  # names(mods) = .set_mods_names(mods)
  if(is.null(by)){ # combine overall     
    combined = list(Overall = 1:ncol(res$Tspace))
  
    } else { # more complex combinations 
    smr=apply(res$summary_table[,by,drop=FALSE],1,paste,collapse=".")    
    uniq_nm = unique(smr)
    combined = lapply(uniq_nm, function(nm) which(smr == nm))
        names(combined) = uniq_nm
    }
  }
  res = lapply(1:length(combined), .npc2jointest, 
               mods = mods, combined = combined, tail = tail, comb_funct = comb_funct)
  names(res) = names(combined)
  res=list(Tspace=jointest:::.get_all_Tspace(res),summary_table=jointest:::.get_all_summary_table(res))
  class(res) <- unique(c("jointest", class(res)))
  res
}


###################
#' @export
#' @describeIn combine \code{combine_contrasts} combines the tests derived from the contrasts of a factor variable to get a global test for the factor (i.e. categorical predictor). It has strong analogies with anova test.

combine_contrasts <- function (mods, comb_funct = "maxT", tail = 0) 
{
  # names(mods) = .set_mods_names(mods)
  # combined=paste(mods$summary_table$Model,mods$summary_table$.assign,sep="_")
  # if (is.null(combined)){
  #   warning('There is not column $summary_table$.assign, nothing is done.' )
  # }
  # uniq_nm = unique(combined)
  # combined = lapply(uniq_nm, function(nm) which(combined == nm))
  # names(combined) = uniq_nm
  # 
  # res = lapply(1:length(combined), .npc2jointest, 
  #              mods = mods, combined = combined, tail = tail, comb_funct = comb_funct)
  # names(res) = names(combined)
  # res=list(Tspace=.get_all_Tspace(res),summary_table=.get_all_summary_table(res))
  # class(res) <- unique(c("jointest", class(res)))
  # res
  smr=apply(res$summary_table[,c("Model",".assign"),drop=FALSE],1,paste,collapse=".")
  new_names=sapply(unique(smr),function(x).find_common_pattern(res$summary_table$Coeff[smr==x]))
  res=combine(res,by=c("Model",".assign"),comb_funct = comb_funct, tail = tail)
  res$summary_table$Coeff=new_names
  res
}

#######################
.npc2jointest <- function (id, mods, combined, tail, comb_funct) 
{
  comb_name = names(combined)[id]
  if (is.null(comb_name)) 
    comb_name = "Combined"
  Tspace = jointest:::npc(mods$Tspace[, combined[[id]],drop=FALSE], comb_funct = comb_funct, 
               tail = tail)
  colnames(Tspace) = comb_name
  Coeff=mods$summary_table[combined[[id]],"Coeff"]
  Coeff=unique(Coeff)
  if(length(Coeff)>1) Coeff="many"
  summary_table = data.frame(Coeff = Coeff, Stat = comb_funct, 
                             nMods = max(1, length(combined[[id]])), S = Tspace[1], 
                             p = jointest:::.t2p_only_first(Tspace, tail = 1))
  list(Tspace = Tspace, summary_table = summary_table)
}

