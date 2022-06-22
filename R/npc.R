npc <- function(Tspace,comb_funct=comb_funct_fisher,tail=0,comb_name="Combined"){
  if(!(comb_funct%in%c("mean","median")))
    Tspace=.set_tail(Tspace,tail=tail)
  
  if(comb_funct%in%c("Fisher","Liptak","Stoufer","Tippet","minp")){
    Tspace=.t2p(Tspace,tail=tail)
  }
  
  nMods= max(1,ncol(Tspace))
  
  if(comb_funct=="mean"){
    Tspace=rowMeans(Tspace)
    Tspace=.set_tail(Tspace,tail=tail)
  } else if(comb_funct=="maxT"){
    Tspace=.rowMax(Tspace)
  } else if(comb_funct=="median"){
    Tspace=.rowMedians(Tspace)
    Tspace=.set_tail(Tspace,tail=tail)
  } else if(comb_funct=="Fisher"){
    Tspace=rowSums(.comb_funct_fisher(Tspace))
  } else if(comb_funct%in%c("Liptak","Stoufer")){
    Tspace=rowSums(.comb_funct_liptak(Tspace))
  } else if(is.function(comb_funct)){
    Tspace=comb_funct(Tspace)
    comb_funct="custom"
  }
  #it is now the vector of combined test stat
  Tspace=matrix(Tspace)
  colnames(Tspace)=comb_name
  
  summary_table=data.frame(Coeff=comb_name,Stat=comb_funct,
             nMods= nMods,
             S=Tspace[1],
             p=.t2p_only_first(Tspace,tail = 1))
  list(Tspace=Tspace, summary_table=summary_table)
}

.rowMedians <- function(X,...)
  apply(X,1,median,...)

.rowMax <- function(X,...)
  apply(X,1,max,...)

.comb_funct_fisher <- function(p)-log(p)
.comb_funct_liptak <- function(p)-qnorm(p)

################################
