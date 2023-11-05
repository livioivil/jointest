#' (nonparametric) combination of jointest object 
#' @param mods object of class \code{jointest} (it can be a list of glm or flipscores converted in a \code{jointest} object using \code{as.jointest})
#' @param comb_funct  Combining function to be used. Several functions are implemented: "mean", "median", "Fisher", "Liptak", (equal to) "Stoufer", "Tippet", (equal to) "minp", "maxT" (the default). 
#' Alternativelly it can be a custom function that has a matrix as input. The function return a vector of length equal to the number of rows of the input matrix. 
#' @param combined a) if \code{NULL} it combines all coefficients with the same names along models, 
#' b) if a vector of characters, it gives a combination for each stats with these names 
#' c) if a list of arrays, each array has two columns reporting model (first column) 
#' and name of the coefficient to be combined.
#' @param tail direction of the alternative hypothesis. It can be "two.sided" (or 0, the default), "less" (or -1) or "greater" (or +1)
#' @export
#' 
combine <- function (mods, comb_funct = "maxT", combined = NULL, tail = 0) 
{
  names(mods) = .set_mods_names(mods)
  smr = .get_all_tested_coeffs_names(mods)
  if (is.null(combined)) {
    uniq_nm = unique(smr)
    if (length(uniq_nm) == length(smr)) {
      combined = list(Overall = smr)
    }
    else {
      combined = lapply(uniq_nm, function(nm) which(smr == 
                                                      nm))
      names(combined) = uniq_nm
    }
  }
  else if (!is.list(combined)) {
    uniq_nm = combined
    combined = lapply(uniq_nm, function(nm) which(smr == 
                                                    nm))
    names(combined) = uniq_nm
  }
  else {
    uniq_nm = unique(unlist(combined))
  }
  Tspace = .get_all_Tspace(mods)
  res = lapply(1:length(combined), .npc2jointest, 
               Tspace = Tspace, combined = combined, tail = tail, comb_funct = comb_funct)
  names(res) = names(combined)
  class(res) <- unique(c("jointest", class(res)))
  res
}

.npc2jointest <- function (id, Tspace, combined, tail, comb_funct) 
{
  comb_name = names(combined)[id]
  if (is.null(comb_name)) 
    comb_name = "Combined"
  Tspace = npc(Tspace[, combined[[id]]], comb_funct = comb_funct, 
                          tail = tail)
  colnames(Tspace) = comb_name
  summary_table = data.frame(Coeff = comb_name, Stat = comb_funct, 
                             nMods = max(1, length(combined[[id]])), S = Tspace[1], 
                             p = .t2p_only_first(Tspace, tail = 1))
  list(Tspace = Tspace, summary_table = summary_table)
}