npc <- function(Tspace,comb_funct=comb_funct_fisher,tail=0){
  if(!(comb_funct%in%c("mean","median")))
    Tspace=.set_tail(Tspace,tail=tail)
  
  if(comb_funct%in%c("Fisher","Liptak","Stoufer","Tippet","minp")){
    Tspace=.t2p(Tspace,tail=tail)
  }
  
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
  
 Tspace
}

.rowMedians <- function(X,...)
  apply(X,1,median,...)

.rowMax <- function(X,...)
  apply(X,1,max,...)

.comb_funct_fisher <- function(p)-log(p)
.comb_funct_liptak <- function(p)-qnorm(p)
#add here other functions..
################################
