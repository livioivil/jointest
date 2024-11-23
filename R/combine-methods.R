#' @title Nonparametric combination of \code{jointest} objects
#' @description Methods for combining \code{jointest} objects. 
#' @docType methods
#' @name combine-methods
#' @param mods a \code{jointest} object.
#' @param comb_funct  combining function to be used. 
#' Several functions are implemented: "mean", "median", "Fisher", "Liptak", (equal to) "Stoufer", "Tippet", (equal to) "minp", "maxT", "Mahalanobis". 
#' Alternatively it can be a custom function that has a Tspace matrix as input. 
#' For \code{combine} the default is \code{comb_funct="maxT"},
#' while for \code{combines_contrasts} the default is \code{comb_funct="Mahalanobis"}.
#' @returns The function returns a \code{jointest}-object. 
#' @param by if \code{NULL} (default), it combines all test statistics.  
#' If a characters, it refers to the column's name of \code{summary_table} (and printed by something like \code{summary(mods)}). 
#' The elements with the same value will be combined. If \code{by} is a vector, the values are defined by row-wise concatenation of the values of the columns in \code{by}.
#' The argument is inactive if \code{by_list} is not \code{NULL}.
#' @param by_list NULL (default) or a list of vectors. For each vector of the list it combines test statistics with position given by the element of the vector. 
#' If the vectors in the list are characters, these refer to names(mods$Tspace).
#' @param tail direction of the alternative hypothesis. It can be "two.sided" (or 0, the default), "less" (or -1) or "greater" (or +1).



#' @usage combine(mods, comb_funct = "maxT", by = NULL, by_list=NULL, tail = 0)
#' @description \code{combine} combines the tests derived from multiverse models.
#' @docType methods
#' @rdname combine
#' @export
#' @examples
#' #First example
#' library(jointest)
#' set.seed(123)
#' 
#' #Simulate data
#' n=20
#' D=data.frame(X=rnorm(n),Z1=rnorm(n),Z2=rnorm(n))
#' D$Y=D$Z1+D$X+rnorm(n)
#' 
#' # Run four glms abd combine it in a list
#' mod1=glm(Y~X+Z1+Z2,data=D)
#' mod2=glm(Y~X+poly(Z1,2)+Z2,data=D)
#' mod3=glm(Y~X+poly(Z1,2)+poly(Z2,2),data=D)
#' mod4=glm(Y~X+Z1+poly(Z2,2),data=D)
#' mods=list(mod1=mod1,mod2=mod2,mod3=mod3,mod4=mod4)
#' 
#' # Let us analyze the tests related to coefficient "X" and combine them
#' res=join_flipscores(mods,n_flips = 5000, seed = 1, tested_coeffs = "X")
#' summary(combine(res))


combine <- function (mods, comb_funct = "maxT", by = NULL, by_list=NULL, tail = 0) 
{
  
  # for(i in 1:length(mods))
  #     mods[[i]]$call$data=mods[[i]]$data
  
  if(!is.null(by_list)){
    combined=by_list
  } else    
    { # by_list is not active, let's use by
    
  # names(mods) = .set_mods_names(mods)
  if(is.null(by)){ # combine overall     
    combined = list(Overall = 1:ncol(mods$Tspace))
  
    } else { # more complex combinations 
    smr=apply(mods$summary_table[,by,drop=FALSE],1,paste,collapse=".")    
    uniq_nm = unique(smr)
    combined = lapply(uniq_nm, function(nm) which(smr == nm))
        names(combined) = uniq_nm
    }
  }
  res = lapply(1:length(combined), .npc2jointest, 
               mods = mods, combined = combined, tail = tail, comb_funct = comb_funct)
  names(res) = names(combined)
  res=list(Tspace=.get_all_Tspace(res),summary_table=.get_all_summary_table(res))
  class(res) <- unique(c("jointest", class(res)))
  res
}


#' @usage combine_contrasts(mods, comb_funct = "Mahalanobis", tail = 0)
#' @description \code{combine_contrasts} combines the tests derived from the contrasts of a factor variable to get a 
#' global test for the factor (i.e. categorical predictor). 
#' It has strong analogies with ANOVA test.
#' @docType methods
#' @rdname combine
#' @export
#' @examples
#' # Second (continued) example
#' # flipscores jointly on all models and all coefficients
#' res=join_flipscores(mods,n_flips = 2000)
#' summary(combine(res))
#' summary(combine(res, by="Model"))
#' summary(combine(res, by="Coeff"))
#' res2=combine_contrasts(res)
#' summary(res2)
#' #custom combinations:
#' coeffs=c("(Intercept)","X","Z1","Z2")
#' coeffs_ids=lapply(coeffs,grep,res2$summary_table$Coeff)
#' names(coeffs_ids)=coeffs
#' summary(combine(res2,by_list =   coeffs_ids))


combine_contrasts <- function (mods, comb_funct = "Mahalanobis", tail = 0) 
{
  names(mods) = .set_mods_names(mods)
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
  
  smr=apply(mods$summary_table[,c("Model",".assign"),drop=FALSE],1,paste,collapse=".")
  new_names=sapply(unique(smr),function(x).find_common_pattern(mods$summary_table$Coeff[smr==x]))
  res=combine(mods,by=c("Model",".assign"),comb_funct = comb_funct, tail = tail)
  res$summary_table$Coeff=new_names
  assigns=paste0(".",unique(mods$summary_table$.assign),"$")
  as_ids=lapply(assigns, function(as)  grep(as,res$summary_table$Model))
  for(i in 1:length(assigns)){
    res$summary_table$Model[as_ids[[i]]]=gsub(assigns[i],"",res$summary_table$Model[as_ids[[i]]])
  }
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
                             nTests = max(1, length(combined[[id]])), S = Tspace[1], 
                             p = .t2p_only_first(Tspace, tail = 1))
  list(Tspace = Tspace, summary_table = summary_table)
}

