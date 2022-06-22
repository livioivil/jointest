#' (nonparametric) combination of jointest object 
#' @param combined a) NULL = combines all coeffs with the same names along models, 
#' b) a vector of characters = it gives a combination for each stats with these names 
#' c) a list of arrays = each array has two columns reporting model (first column) 
#' and name of the coefficient to be combined.
#' @export
combine <- function(mods,
                       comb_funct="maxT",
                       combined=NULL,
                       tail=0){

  names(mods)=.set_mods_names(mods)
  
  
  smr=.get_all_tested_coeffs_names(mods)
  if(is.null(combined)){
    uniq_nm=unique(smr)
    if(length(uniq_nm)==length(smr)) {#combine all together
      combined=list(Overall=smr)
      } else {
      combined=lapply(uniq_nm,function(nm) which(smr==nm))
      names(combined)=uniq_nm
    }
  } else  if(!is.list(combined)){ #is a character vector
    uniq_nm=combined
    combined=lapply(uniq_nm,function(nm) which(smr==nm))
    names(combined)=uniq_nm
  } else { #is a list
    uniq_nm=unique(unlist(combined))
  }
  
  Tspace=.get_all_Tspace(mods)
  
  res=lapply(1:length(combined),function(id){
    npc(Tspace[,combined[[id]]],
        comb_funct = comb_funct,
        tail = tail,
        comb_name =names(combined)[id])
    })
  
  # ## I don't now how to set names in npc(), I do it here
  # res=.set_comb_names_in_summary_table(res,names(combined))
  # res=.set_comb_names_in_Tspace(res,names(combined))
  #done
  
  names(res)=names(combined)
  
  # 
  # if(length(res)>1)
  #   res$Overall=npc(Tspace[,uniq_nm],comb_funct = comb_funct,tail = tail)
  class(res) <- c("jointest", class(res))
  res  
  # list(summary_table=summary_ei_combined(res),
  #      Tspace=.get_all_Tspace(res)
  #      )
}