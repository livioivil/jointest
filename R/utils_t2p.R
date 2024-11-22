.t2p <- function(Tspace,tail=0){
  Tspace=.set_tail(Tspace,tail=tail)
  if(is.vector(Tspace)){
    P = rank(-Tspace , ties.method = "max", na.last = "keep")/length(Tspace)
  } else {
    P = apply(-Tspace, 2, rank, ties.method = "max", na.last = "keep")/nrow(Tspace)
    P = as.matrix(P)
  }
  P
}

.t2p_only_first <- function(Tspace,tail=0){
  Tspace=.set_tail(Tspace,tail=tail)
  if(is.vector(Tspace)){
    P = mean(Tspace>=Tspace[1])
  } else if(ncol(Tspace)==1){
    P = mean(Tspace[,]>=Tspace[1,1])
  } else    {
    P = apply(Tspace,2, function(Tsp)mean(Tspace>=Tspace[1]))
  }
  P
}


.set_tail <- function(Tspace,tail=0){
  if((tail==0)|(tail=="two.sided"))   
    Tspace=abs(Tspace) else 
      if((tail<0)|(tail=="less"))
        Tspace=-Tspace
      
      Tspace
}

