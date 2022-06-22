#'Correct p-values of a jointest object for multiple testing (FamilyWise Error Rate)
#'@export

p.adjust.fwer <- function(mods,method = "maxT",tail=0){
  Tspace=as.matrix(.get_all_Tspace(mods))
#  smr=summary_ei(mods)
#  nrm=(smr$Score/smr$eff_size)^-1
#  Tspace=Tspace%*%diag(nrm)
  # sumY2s=.get_all_sumScore2(mods)
  # n=nrow(mods[[1]]$scores)
  # Tspace=sapply(1:ncol(Tspace),function(i) .sum2t(Tspace[,i],sumY2s[i],n))
  colnames(Tspace)=paste0("v",1:ncol(Tspace))
  p.adj=flip::flip.adjust(.set_tail(Tspace,tail = tail),method = method)
  tmp=lapply(1:length(mods),function(id){
    tt=summary.jointest(mods[id])
    tt$p.adj=p.adj[id]
    mods[[id]]$summary_table=tt
    mods[[id]]
  })
  
  class(tmp) <- c("jointest", class(tmp))
  tmp
}
