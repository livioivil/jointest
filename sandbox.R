
set.seed(1)
dt=data.frame(X=rnorm(20),
              Z=factor(rep(LETTERS[1:3],length.out=20)))
dt$Y=rpois(n=20,lambda=exp((dt$Z=="C") + 2*dt$X))
mod=flipscores(Y~Z+X,data=dt,family="poisson",n_flips=1000)
summary(mod)

colSums(mod$scores)
head(mod$Tspace)


score2t <- function(Tspace,nrms=NULL,ns=NULL){
  if(is(Tspace,"flipscores")){
    if(is.null(nrms))
      nrms=attr(Tspace$scores,"nrm")
    if(is.null(n))
      ns=rep(nrow(Tspace$scores),ncol(Tspace$scores))
    Tspace=Tspace$Tspace[,,drop=FALSE]
  }
  
  if(is.null(nrms))
    warning("nrms is missing and can't be retrived from Tspace since it is not a flipscores object")
  if(is.null(n))
    warning("n is missing and can't be retrived from Tspace since it is not a flipscores object")
  
  .convert_score2t <-function(i){
    tspace=Tspace[,i]
    tspace*sqrt(ns[i]/(nrms[i]^2-tspace^2))
  }
  #plot(scale(object$Tspace[,4]),.convert_score2t(4));points(scale(object$Tspace[,4])[1],.convert_score2t(4)[1],col="red")
  temp=sapply(1:ncol(Tspace),
               .convert_score2t)
  Tspace[,]=temp
  Tspace
}

str(mod$Tspace)
head(score2t(mod))



str(res$Tspace)

nrms=sapply(res$mods,function(mod) attr(mod$scores,"nrm"))
ns=sapply(res$mods,function(mod) nrow(mod$scores))
res$Tspace=score2t(res$Tspace,nrms,ns)


temp=sapply(1:ncol(res$Tspace),function(i) 
  score2t(res$Tspace[,i,drop=FALSE],nrms[i],ns[i]))
str(temp)




#########################
library(devtools)
install()

D=data.frame(ID=rep(1:10,each=4),
             wi1=rep(1:2,20),
             wi2=rnorm(40),
             be1=rep(rnorm(10),each=4),
             be2=rep(rnorm(10),each=4),
             y=rnorm(40))

library(jointest)
# Fit the mixed model
model <- flip2sss(y ~ wi1 * wi3 * be1 * be2,cluster = D$ID, 
                  data = D)
