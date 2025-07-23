
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
