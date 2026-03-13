library(flipscores)
set.seed(123)
dt=data.frame(X=rnorm(20),
    Z=factor(rep(LETTERS[1:3],length.out=20)))
  dt$Y=rpois(n=20,lambda=exp((dt$Z=="C") + 2*dt$X))
mod=flipscores(Y~Z+X,data=dt,family="gaussian",n_flips=1000)
summary(mod)
head(mod$Tspace)
mod=flipscores(Y~Z+X,data=dt,,score_type = "effective",
               family="gaussian",n_flips=1000)
summary(mod)
head(mod$Tspace)


summary(mod)$coeff[4,4]
summary.glm(mod)$coeff[4,3]

flipscores:::.sum2t(stat = mod$Tspace[1,4],sumY2 = sum(mod$scores[,4]^2),n = nrow(mod$scores))
.score2t(mod$Tspace[1,4],attr(mod$scores,"nrm")[4],16)

flipscores:::.sum2t(stat = mod$Tspace[1,4],sumY2 = attr(mod$scores,"nrm")[4]^2,n = nrow(mod$scores))


scr2t=.score2t(mod$Tspace[,4],attr(mod$scores,"nrm")[4],20)
sum2t=flipscores:::.sum2t(stat = mod$Tspace[,4],sumY2 = sum(mod$scores[,4]^2),n = nrow(mod$scores))
plot(sum2t,scr2t)

############################
set.seed(1)
dt=data.frame(X=rnorm(20),
              Z=factor(rep(LETTERS[1:3],length.out=20)))
dt$Y=rpois(n=20,lambda=exp((dt$Z=="C") + 2*dt$X))
mod=flipscores(Y~Z+X,data=dt,family="poisson",n_flips=1000)
summary(mod)

colSums(mod$scores)
head(mod$Tspace)



str(mod$Tspace)
head(score2t(mod))



str(res$Tspace)

nrms=sapply(res$mods,function(mod) attr(mod$scores,"nrm"))
ns=sapply(res$mods,function(mod) nrow(mod$scores))
res$Tspace=flipscores:::score2t(res$Tspace,nrms,ns)


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
